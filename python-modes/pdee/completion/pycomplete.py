"""
Python dot expression completion using Pymacs.

This almost certainly needs work, but if you add

    (require 'pycomplete)

to your init.el file and have Pymacs installed, when you hit M-TAB it will
try to complete the dot expression before point.  For example, given this
import at the top of the file:

    import time

typing "time.cl" then hitting M-TAB should complete "time.clock".

This is unlikely to be done the way Emacs completion ought to be done, but
it's a start.  Perhaps someone with more Emacs mojo can take this stuff and
do it right.

See pycomplete.el for the Emacs Lisp side of things.

Most of the public functions in this module have the signature

(s, fname=None, imports=None)

where s is the symbol to complete, fname is the file path and imports
the list of import statements to use. The fname parameter is used as a
key to cache the global and local context and the symbols imported or
evaluated so far. The cache for an fname is cleared when its imports
are changed. When not passing a list of imports (or None), the currently
used imports are preserved. The caching should make subsequent operations
(e.g. another completion or signature lookup after a completion) less
expensive.
"""

# Original Author:     Skip Montanaro <skip@pobox.com>
# Maintainer: Urs Fleisch <ufleisch@users.sourceforge.net>
# Created:    Oct 2004
# Keywords:   python pymacs emacs

# This software is provided as-is, without express or implied warranty.
# Permission to use, copy, modify, distribute or sell this software, without
# fee, for any purpose and by any individual or organization, is hereby
# granted, provided that the above copyright notice and this paragraph
# appear in all copies.

# Along with pycomplete.el this file allows programmers to complete Python
# symbols within the current buffer.

import sys
import types
import inspect
import keyword
import StringIO
import os
import pydoc
import ast

try:
    x = set
except NameError:
    from sets import Set as set
else:
    del x

class ImportExtractor(ast.NodeVisitor):
    """NodeVisitor to extract the top-level import statements from an AST.

    To generate code containing all imports in try-except statements,
    call get_import_code(node), where node is a parsed AST.
    """
    def visit_FunctionDef(self, node):
        # Ignore imports inside functions or methods.
        pass

    def visit_ClassDef(self, node):
        # Ignore imports inside classes.
        pass

    def generic_visit(self, node):
        # Store import statement nodes.
        if isinstance(node, (ast.Import, ast.ImportFrom)):
            self._import_nodes.append(node)
        ast.NodeVisitor.generic_visit(self, node)

    def get_import_code(self, node, fname='<string>'):
        """Get compiled code of all top-level import statements found in the
        AST of node."""
        self._import_nodes = []
        self.visit(node)
        body = []
        for imp_node in self._import_nodes:
            body.append(ast.TryExcept(body=[imp_node], handlers=[
                ast.ExceptHandler(type=None, name=None, body=[ast.Pass()])],
                orelse=[]))
        node = ast.Module(body=body)
        ast.fix_missing_locations(node)
        code = compile(node, fname, 'exec')
        return code


class CodeRemover(ast.NodeTransformer):
    """NodeTransformer which replaces function statements with 'pass'
    and keeps only safe assignments, so that the resulting code can
    be used for code completion.

    To reduce the code from the node of a parsed AST, call
    get_transformed_code(node).
    """
    def visit_FunctionDef(self, node):
        # Replace all function statements except doc string by 'pass'.
        if node.body:
            if isinstance(node.body[0], ast.Expr) and \
               isinstance(node.body[0].value, ast.Str):
                # Keep doc string.
                first_stmt = node.body[1]
                node.body = [node.body[0]]
            else:
                first_stmt = node.body[0]
                node.body = []
            node.body.append(ast.copy_location(ast.Pass(), first_stmt))
            return node
        return None

    def visit_Expr(self, node):
        # Remove all expressions except strings to keep doc strings.
        if isinstance(node.value, ast.Str):
            return node
        return None

    @staticmethod
    def replace_unsafe_value(node, replace_self=None):
        """Modify value from assignment if unsafe.

        If replace_self is given, only assignments starting with 'self.' are
        processed, the assignment node is returned with 'self.' replaced by
        the value of replace_self (typically the class name).
        For other assignments, None is returned."""
        if replace_self:
            if len(node.targets) != 1:
                return None
            target = node.targets[0]
            if isinstance(target, ast.Attribute) and \
               isinstance(target.value, ast.Name) and \
               target.value.id == 'self' and \
               isinstance(target.value.ctx, ast.Load):
                node.targets[0].value.id = replace_self
            else:
                return None
        if isinstance(node.value, (ast.Str, ast.Num)):
            pass
        elif isinstance(node.value, (ast.List, ast.Tuple)):
            node.value.elts = []
        elif isinstance(node.value, ast.Dict):
            node.value.keys = []
            node.value.values = []
        elif isinstance(node.value, ast.ListComp):
            node.value = ast.copy_location(ast.List(elts=[], ctx=ast.Load()), node.value)
        else:
            node.value = ast.copy_location(ast.Name(id='None', ctx=ast.Load()), node.value)
        return node

    def visit_Assign(self, node):
        # Replace unsafe values of assignements by None.
        return self.replace_unsafe_value(node)

    def visit_Name(self, node):
        # Pass names for bases in ClassDef.
        return node

    def visit_Attribute(self, node):
        # Pass attributes for bases in ClassDef.
        return node

    def visit_ClassDef(self, node):
        # Visit nodes of class.
        # Store instance member assignments to be added later to generated code.
        self_assignments = {}
        for child in ast.walk(node):
            if isinstance(child, ast.Assign):
                new_child = self.replace_unsafe_value(child,
                                                      replace_self=node.name)
                if new_child:
                    new_var = child.targets[0].attr
                    old_assign = self_assignments.get(new_var)
                    if not old_assign or (
                            isinstance(old_assign.value, ast.Name) and
                            old_assign.value.id == 'None'):
                        self_assignments[new_var] = new_child
        self._class_assignments.extend(self_assignments.values())
        return ast.NodeTransformer.generic_visit(self, node)

    def visit_Module(self, node):
        # Visit nodes of module
        return ast.NodeTransformer.generic_visit(self, node)

    def generic_visit(self, node):
        # Remove everything which is not handled by the methods above
        return None

    def get_transformed_code(self, node, fname='<string>'):
        """Get compiled code containing only empty functions and methods
        and safe assignments found in the AST of node."""
        self._class_assignments = []
        node = self.visit(node)
        # The self members are added as attributes to the class objects
        # rather than included as class variables inside the class definition
        # so that names starting with '__' are not mangled.
        node.body.extend(self._class_assignments)
        code = compile(node, fname, 'exec')
        return code


class PyCompleteDocument(object):
    """Completion data for Python source file."""
    _helpout = StringIO.StringIO
    _stdout = sys.stdout

    _instances = {}

    def __init__(self, fname=None):
        """Constructor for internal use.
        The factory method instance() shall be used instead.
        """
        self._fname = fname
        self._imports = None
        self._locald = {}
        self._globald = globals()
        self._symnames = []
        self._symobjs = {}
        self._parse_source_called = False

    @classmethod
    def instance(cls, fname):
        """Get PyCompleteDocument object for fname.
        If no object for this file name exists, a new object is created and
        registered.
        """
        obj = cls._instances.get(fname)
        if obj is None:
            obj = PyCompleteDocument(fname)
            cls._instances[fname] = obj
        return obj

    def _import_modules(self, imports):
        """Import modules using the statements in imports.
        If the imports are the same as in the last call, the methods
        immediately returns, also if imports is None.
        """
        if imports is None and not self._parse_source_called:
            self.parse_source()
        if imports is None or imports == self._imports:
            return
        # changes to where the file is
        if self._fname:
            os.chdir(os.path.dirname(self._fname))
        self._locald = {}
        self._symnames = []
        self._symobjs = {}
        for stmt in imports:
            try:
                exec stmt in self._globald, self._locald
            except TypeError:
                raise TypeError, 'invalid type: %s' % stmt
            except Exception:
                continue
        self._imports = imports

    def _collect_symbol_names(self):
        """Collect the global, local, builtin symbols in _symnames.
        If _symnames is already set, the method immediately returns.
        """
        if not self._symnames:
            keys = set(keyword.kwlist)
            keys.update(self._locald.keys())
            keys.update(self._globald.keys())
            import __builtin__
            keys.update(dir(__builtin__))
            self._symnames = list(keys)
            self._symnames.sort()

    def _get_symbol_object(self, s):
        """Get a symbol by evaluating its name or importing a module
        or submodule with the name s.
        """
        sym = self._symobjs.get(s)
        if sym is not None:
            return sym
        # changes to where the file is
        if self._fname:
            os.chdir(os.path.dirname(self._fname))
        try:
            sym = eval(s, self._globald, self._locald)
        except NameError:
            try:
                sym = __import__(s, self._globald, self._locald, [])
                self._locald[s] = sym
            except ImportError:
                pass
        except AttributeError:
            try:
                sym = __import__(s, self._globald, self._locald, [])
            except ImportError:
                pass
        except SyntaxError:
            pass
        if sym is not None:
            self._symobjs[s] = sym
        return sym

    def _load_symbol(self, s, strict=False):
        """Get a symbol for a dotted expression.

        Returns the last successfully found symbol object in the
        dotted chain. If strict is set True, it returns True as
        soon as a symbol is not found. Therefore strict=True can
        be used to find exactly the symbol for s, otherwise a
        symbol for a parent can be returned, which may be enough
        if searching for help on symbol.
        """
        sym = self._symobjs.get(s)
        if sym is not None:
            return sym
        dots = s.split('.')
        if not s or len(dots) == 1:
            sym = self._get_symbol_object(s)
        else:
            for i in range(1, len(dots) + 1):
                s = '.'.join(dots[:i])
                if not s:
                    continue
                sym_i = self._get_symbol_object(s)
                if sym_i is not None:
                    sym = sym_i
                elif strict:
                    return None
        return sym

    def _get_help(self, s, imports=None):
        """Return string printed by help function."""
        if not s:
            return ''
        if s == 'pydoc.help':
            # Prevent pydoc from going into interactive mode
            s = 'pydoc.Helper'
        obj = None
        if not keyword.iskeyword(s):
            try:
                self._import_modules(imports)
                obj = self._load_symbol(s, strict=False)
            except Exception, ex:
                return '%s' % ex
        if not obj:
            obj = str(s)
        out = self._helpout()
        try:
            sys.stdout = out
            pydoc.help(obj)
        finally:
            sys.stdout = self._stdout
        return out.getvalue()

    @staticmethod
    def _find_constructor(class_ob):
        """Given a class object, return a function object used for the
        constructor (ie, __init__() ) or None if we can't find one."""
        try:
            return class_ob.__init__.im_func
        except AttributeError:
            for base in class_ob.__bases__:
                rc = PyCompleteDocument._find_constructor(base)
                if rc is not None:
                     return rc
        return None

    def get_all_completions(self, s, imports=None):
        """Return contextual completion of s (string of >= zero chars).

        If given, imports is a list of import statements to be executed
        first.
        """
        self._import_modules(imports)

        last_dot_pos = s.rfind('.')
        if last_dot_pos == -1:
            self._collect_symbol_names()
            if s:
                return [k for k in self._symnames if k.startswith(s)]
            else:
                return self._symnames

        sym = self._load_symbol(s[:last_dot_pos], strict=True)
        if sym is not None:
            s = s[last_dot_pos + 1:]
            return [k for k in dir(sym) if k.startswith(s)]
        return []

    def complete(self, s, imports=None):
        """Complete symbol if unique, else return list of completions."""
        if not s:
            return ''

        completions = self.get_all_completions(s, imports)
        if len(completions) == 0:
            return None
        else:
            dots = s.split(".")
            prefix = os.path.commonprefix([k for k in completions])
            if len(completions) == 1 or len(prefix) > len(dots[-1]):
                return [prefix[len(dots[-1]):]]
            return completions

    def help(self, s, imports=None):
        """Return help on object."""
        try:
            return self._get_help(s, imports)
        except Exception, ex:
            return '%s' % ex

    def get_docstring(self, s, imports=None):
        """Return docstring for symbol s."""
        if s and not keyword.iskeyword(s):
            try:
                self._import_modules(imports)
                obj = self._load_symbol(s, strict=True)
                if obj and not isinstance(obj, (int, float, basestring)):
                    doc = inspect.getdoc(obj)
                    if doc:
                        return doc
            except:
                pass
        return ''

    def get_signature(self, s, imports=None):
        """Return info about function parameters."""
        if not s or keyword.iskeyword(s):
            return ''
        obj = None
        sig = ""

        try:
            self._import_modules(imports)
            obj = self._load_symbol(s, strict=False)
        except Exception, ex:
            return '%s' % ex

        if type(obj) in (types.ClassType, types.TypeType):
            # Look for the highest __init__ in the class chain.
            ctr = self._find_constructor(obj)
            if ctr is not None:
                obj = ctr
        elif type(obj) == types.MethodType:
            # bit of a hack for methods - turn it into a function
            # but we drop the "self" param.
            obj = obj.im_func

        if type(obj) in [types.FunctionType, types.LambdaType]:
            (args, varargs, varkw, defaults) = inspect.getargspec(obj)
            sig = ('%s: %s' % (obj.__name__,
                               inspect.formatargspec(args, varargs, varkw,
                                                     defaults)))
        doc = getattr(obj, '__doc__', '')
        if doc and not sig:
            doc = doc.lstrip()
            pos = doc.find('\n')
            if pos < 0 or pos > 70:
                pos = 70
            sig = doc[:pos]
        return sig

    def get_location(self, s, imports=None):
        """Return file path and line number of symbol, None if not found."""
        if not s or keyword.iskeyword(s):
            return None
        try:
            self._import_modules(imports)
            obj = self._load_symbol(s, strict=False)
            if obj is not None:
                if type(obj) in (types.ClassType, types.TypeType):
                    obj = obj.__init__
                if type(obj) == types.MethodType:
                    obj = obj.im_func
                if type(obj) in [types.FunctionType, types.LambdaType]:
                    code = obj.func_code
                    return (os.path.abspath(code.co_filename), code.co_firstlineno)
                # If not found, try using inspect.
                return (inspect.getsourcefile(obj), inspect.getsourcelines(obj)[1])
        except:
            pass
        return None

    def parse_source(self, only_reload=False):
        """Parse source code to get imports and completions.

        If this method is called, the imports parameter for the other methods
        must be omitted (or None), so that the imports are taken from the
        parsed source code. If only_reload is True, the source is only parsed
        if it has been parsed before.
        None is returned if OK, else a string describing the error.
        """
        if only_reload and not self._parse_source_called:
            return None
        self._parse_source_called = True
        if not self._fname:
            return None

        try:
            with open(self._fname) as fh:
                src = fh.read()
        except IOError, ex:
            return '%s' % ex

        # changes to where the file is
        os.chdir(os.path.dirname(self._fname))

        try:
            node = ast.parse(src, self._fname)
        except (SyntaxError, TypeError), ex:
            return '%s' % ex
        import_code = ImportExtractor().get_import_code(node, self._fname)

        old_globald = self._globald.copy()
        old_locald = self._locald
        self._locald = {}
        try:
            exec import_code in self._globald, self._locald
        except Exception, ex:
            self._globald = old_globald
            self._locald = old_locald
            return '%s' % ex

        self._symnames = []
        self._symobjs = {}

        reduced_code = CodeRemover().get_transformed_code(node, self._fname)
        try:
            exec reduced_code in self._globald, self._locald
        except Exception, ex:
            return '%s' % ex
        return None

def get_all_completions(s, fname=None, imports=None):
    """Get a list of possible completions for s.

    The completions extend the expression s after the last dot.
    """
    return PyCompleteDocument.instance(fname).get_all_completions(
        s, imports)

def pycomplete(s, fname=None, imports=None):
    """Complete the Python expression s.

    If multiple completions are found, a list of possible completions
    (names after the last dot) is returned.
    If one completion is found, a list with a string containing the
    remaining characters is returned.
    If no completion is found, None is returned.
    """
    return PyCompleteDocument.instance(fname).complete(s, imports)

def pyhelp(s, fname=None, imports=None):
    """Return help on object s."""
    return PyCompleteDocument.instance(fname).help(s, imports)

def pydocstring(s, fname=None, imports=None):
    """Return docstring of symbol."""
    return PyCompleteDocument.instance(fname).get_docstring(s, imports)

def pysignature(s, fname=None, imports=None):
    """Return info about function parameters."""
    return PyCompleteDocument.instance(fname).get_signature(s, imports)

def pylocation(s, fname=None, imports=None):
    """Return file path and line number of symbol, None if not found."""
    return PyCompleteDocument.instance(fname).get_location(s, imports)

def parse_source(fname, only_reload=False):
    """Parse source code to get imports and completions.

    If this function is called, the imports parameter for the other functions
    must be omitted (or None), so that the imports are taken from the
    parsed source code. If only_reload is True, the source is only parsed if
    it has been parsed before.
    """
    return PyCompleteDocument.instance(fname).parse_source(only_reload)

if __name__ == "__main__":
    print "<empty> ->", pycomplete("")
    print "sys.get ->", pycomplete("sys.get")
    print "sy ->", pycomplete("sy")
    print "sy (sys in context) ->", pycomplete("sy", imports=["import sys"])
    print "foo. ->", pycomplete("foo.")
    print "Enc (email * imported) ->",
    print pycomplete("Enc", imports=["from email import *"])
    print "E (email * imported) ->",
    print pycomplete("E", imports=["from email import *"])

    print "Enc ->", pycomplete("Enc")
    print "E ->", pycomplete("E")

# Local Variables :
# pymacs-auto-reload : t
# End :

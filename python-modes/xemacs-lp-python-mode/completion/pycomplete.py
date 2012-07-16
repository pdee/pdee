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

try:
    x = set
except NameError:
    from sets import Set as set
else:
    del x

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
                obj = self._load_symbol(s, strict=False)
                if obj:
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
        except:
            pass
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

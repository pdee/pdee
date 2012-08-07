#!/usr/bin/env python

import os
import linecache
import tempfile
from pycomplete import *

def test_signature():
    assert pysignature('os.path.join') == 'join: (a, *p)'
    assert pysignature('urllib.urlopen') == \
           'urlopen: (url, data=None, proxies=None)'
    assert pysignature('httplib.HTTPConnection.request') == \
           'request: (self, method, url, body=None, headers={})'
    assert pysignature('httplib.HTTPMessage') == \
           '__init__: (self, fp, seekable=1)'
    assert pysignature('httplib.HTTPResponse') == \
           '__init__: (self, sock, debuglevel=0, strict=0, method=None, buffering=False)'
    assert pysignature('xml.dom.minidom.parse') == \
           'parse: (file, parser=None, bufsize=None)'
    assert pysignature('csv.reader') == \
           'csv_reader = reader(iterable [, dialect=\'excel\']'
    assert pysignature('super') == 'super(type) -> unbound super object'

def test_help():
    assert pyhelp('os.path.join').startswith('Help on function join')
    assert pyhelp('logging', imports=('import logging',)).startswith(
        'Help on package logging:\n\nNAME\n    logging\n')
    assert pyhelp('csv', imports=('import csv',)).startswith(
        'Help on module csv:\n\nNAME\n    csv - CSV parsing and writing.\n')
    assert pyhelp('import').startswith(
        'The ``import`` statement\n************************\n')
    assert pyhelp('pydoc.help').startswith(
        'Help on class Helper in module pydoc')

def test_complete():
    assert pycomplete('') == ''
    assert pycomplete('sys.get') == [
        'getcheckinterval', 'getdefaultencoding', 'getdlopenflags',
        'getfilesystemencoding', 'getprofile', 'getrecursionlimit',
        'getrefcount', 'getsizeof', 'gettrace']
    assert pycomplete('set') == ['set', 'setattr']
    assert pycomplete('settr') is None
    assert pycomplete('settr', imports=['from sys import settrace']) == [
        'ace']
    assert pycomplete('foo.') is None
    assert pycomplete('Enc') is None
    assert pycomplete('Enc', imports=['from email import *']) == ['oders']
    assert pycomplete('E') == [
        'EOFError', 'Ellipsis', 'Encoders', 'EnvironmentError', 'Errors',
        'Exception']

def test_completions():
    assert get_all_completions('os.path.jo') == ['join']
    assert get_all_completions('settr', imports=['']) == []
    assert get_all_completions('settr',
                imports=['from sys import settrace']) == ['settrace']
    # Check if imports are still cached.
    assert get_all_completions('settr', imports=None) == ['settrace']
    # Change list of imports, so that cache is invalidated.
    assert get_all_completions('settr', imports=['']) == []

def test_location():
    fn, line = pylocation('os.path.join')
    assert os.path.exists(fn)
    assert linecache.getline(fn, line).startswith('def join')
    assert pylocation('cStringIO.StringIO') is None
    fn, line = pylocation('urllib')
    assert os.path.exists(fn)

def test_docstring():
    assert pydocstring('os.path.abspath') == 'Return an absolute path.'
    assert pydocstring('os.path').startswith(
        'Common operations on Posix pathnames.\n')
    assert pydocstring('httplib.HTTPMessage.getheader').startswith(
        'Get the header value for a name.\n')
    assert pydocstring('yield') == ''
    assert pydocstring('numbers.Real.real') == \
      'Real numbers are their real component.'
    assert pydocstring('notexisting') == ''
    assert pydocstring('re.IGNORECASE') == ''

def test_parse_source():
    tmp_file = tempfile.NamedTemporaryFile(suffix='.py')
    name = tmp_file.name
    with tmp_file.file as fh:
        assert parse_source('not_existing') == \
           "[Errno 2] No such file or directory: 'not_existing'"
        assert parse_source(name) is None
        # Nothing imported so far
        assert get_all_completions('dat' , name) == []
        src = """
"Doc for module."
import os
from datetime import date, \
time
if os.getenv('LC'):
    import linecache

def testfunc():
    "Doc for testfunc."
    import urllib

class TestClass(date):
    "Doc for TestClass."
    CONST1 = 7
    CONST2 = 'abc'
    CONST3 = ['a', ]
    CONST4 = open('not_existing')

    def __init__(self):
        self._member1 = 'string member'
        self._member2 = None
        self.__member3 = [ None, open('not_existing') ]
    def testmeth(self, arg1=1):
        "Doc for testmeth."
        print 'From testmeth %d' % arg1
        if arg1 == 2:
            self._member1 = None
    @staticmethod
    def teststaticmeth(arg1=2):
        "Doc for teststaticmeth."
        print 'From teststaticmeth %d' % arg1
    @classmethod
    def testclassmeth(cls, arg1=3):
        "Doc for testclassmeth."
        open('not_existing')
    @property
    def testprop(self):
        "Doc for testprop."
        return 4 * 8

if __name__ == '__main__':
    testfunc()
"""
        num_src_lines = len(src.splitlines())
        fh.write(src)
        fh.flush()
        assert parse_source(name) == None
        # Check if only global imports are visible
        assert get_all_completions('dat' , name) == ['date']
        assert get_all_completions('tim' , name) == ['time']
        assert get_all_completions('url' , name) == []
        assert get_all_completions('line' , name) == ['linecache']
        assert get_all_completions('os' , name) == ['os']
        # Check for definitions in local file
        assert get_all_completions('test' , name) == ['testfunc']
        assert get_all_completions('TestClass.CO' , name) == \
          ['CONST1', 'CONST2', 'CONST3', 'CONST4']
        assert get_all_completions('TestClass.test' , name) == \
          ['testclassmeth', 'testmeth', 'testprop', 'teststaticmeth']
        # Check for instance members
        assert get_all_completions('TestClass._mem', name) == \
          ['_member1', '_member2']
        assert get_all_completions('TestClass.__mem', name) == ['__member3']
        assert get_all_completions('TestClass._member1.start', name) == \
          ['startswith']
        assert get_all_completions('TestClass._member2.', name) == []
        assert get_all_completions('TestClass.__member3.ext', name) == \
          ['extend']
        assert pydocstring('TestClass._member1', name) == ''
        assert pydocstring('TestClass._member2', name) == ''
        assert pydocstring('TestClass.__member3', name) == ''
        # Check for super class
        assert get_all_completions('TestClass.week' , name) == ['weekday']
        assert get_all_completions('TestClass.utc' , name) == []
        # Check signature, documentation and location
        assert pysignature('TestClass.testmeth', name) == \
          'testmeth: (self, arg1=1)'
        assert pydocstring('testfunc', name) == \
          'Doc for testfunc.'
        assert pylocation('TestClass.testclassmeth', name) == \
          (name, num_src_lines - 10)
        # Verify that loaded symbols are not affected by transient
        # syntax error
        fh.write('while')
        fh.flush()
        assert parse_source(name) == \
          'invalid syntax (%s, line %d)' % (os.path.basename(name),
                                            num_src_lines + 1)
        assert get_all_completions('dat' , name) == ['date']
        # Replace file contents and check new imports
        fh.seek(0)
        fh.truncate(0)
        fh.write('import urllib\n')
        fh.flush()
        assert parse_source(name) == None
        assert get_all_completions('dat' , name) == []
        assert get_all_completions('url' , name) == ['urllib']

def run_tests():
    test_complete()
    test_completions()
    test_help()
    test_signature()
    test_location()
    test_docstring()
    test_parse_source()

if __name__ == "__main__":
    run_tests()

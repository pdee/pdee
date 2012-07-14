#!/usr/bin/env python

import os
import linecache
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

def run_tests():
    test_complete()
    test_completions()
    test_help()
    test_signature()
    test_location()

if __name__ == "__main__":
    run_tests()

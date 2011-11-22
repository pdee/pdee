from unittest import TestLoader, TestResult
from Pymacs import lisp, Let
import sys
import os

interactions = {}
def discover(directory):
    directory = os.path.expanduser(directory) # The tilde does not work with os.chdir
    os.chdir(directory)
    
    # Discovering tests using unittest framework
    loader = TestLoader()
    tests = loader.discover(directory, top_level_dir=directory)
    result = EmacsTestResult()
    
    # Create a buffer (if it not exists) and put the formatted results
    # inside it
    let = Let()
    lisp.get_buffer_create("unittest")
    let.push_excursion()
    lisp.set_buffer("unittest")
    lisp.erase_buffer()
    tests.run(result)
    lisp.insert("\n")
    lisp.insert("Errors:\n")
    for test, traceback in result.errors:
        lisp.insert(str(test))
        lisp.insert(traceback)
    let.pop_excursion()
    
    lisp.pop_to_buffer("unittest")
    lisp.compilation_mode()
    lisp.beginning_of_buffer()
    
class EmacsTestResult(TestResult):
    """ Result object to handle the testing
    """
    def addSuccess(self, test):
        super(EmacsTestResult, self).addSuccess(test)
        lisp.insert(".")
        
    def addFailure(self, test, err):
        super(EmacsTestResult, self).addFailure(test, err)
        lisp.insert("F")
    def addError(self, test, err):
        super(EmacsTestResult, self).addError(test, err)
        lisp.insert("E")

interactions[discover] = 'DProject Directory: '

# Local Variables:
# pymacs-auto-reload: t
# End:
#! /usr/bin/env python
# -*- coding: utf-8 -*-


# https://bugs.launchpad.net/bugs/328807
def foo():
    x = 1
# indentation is wrong after this comment
y = 1



# https://bugs.launchpad.net/python-mode/+bug/328813
print """ "Hi!" I'm a doc string"""
print ''' 'Hi!' I'm a doc string'''
print """ ''' "Hi!" I'm a doc string ''' """
print ''' """ "Hi!" I'm a doc string """ '''

  """
       "Hi!" I'm a doc string
    """



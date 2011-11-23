# Pymacs
## all around running Pymacs \w PDEE

### Pymacs(the Python part) needed?

- Do I need to install pymacs(the python part) to use this package?

I now that pymacs is in 'python-libs/Pymacs', but when I start emacs, I get the error

    error: Pymacs helper did not start within 30 seconds

which seems to be because pymacs.py isn't found?
Is there a path thats needs to be set? And is there a difference whether python2/python3 is used?
I run archlinux, which have python=python3

- No you don't need pymacs on your part but you should add  this line in your .bashrc if you're using Arch (like me!)

    export PYMACS_PYTHON=python2




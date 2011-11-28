# Objectives

Provide a self-contained collection of python helpers, IDE-like. 

## Project management

The candidate default (emacs24) project management library in emacs is cedet, it would be appropriate to extend it if possible.

## Non-Project management

pythoneers like to write single-file scripts, all the following features should be there (in some, probably restricted way) without having the hassle to init a full-fledget project.

## Code Completion

This one is complex, and there are multiple approaches:

ropemacs: this one provides the best completion probably but it suffers of some problems. 
                 1) slowness 
                 2) it requires pymacs 
                 3) has its own project management and refactory tools with their own interface
To integrate ropemacs in an “emacsy” environment one should heavy-edit the ropemacs code.

pysmell: it’s a static code checker that provides tags for python, it seems to be good but I think it has not what’s called “intellisense”.

cedet-semantic: they have a sort of python parser but I don’t know if working on it is viable, I believe this would be definitely the most clean and standard-compliant solution (that will surely adapt with most of the completion-ui libraries out there).


## Syntax Checking

The actual pyflakes + flymake combo does a pretty good job. It’s fast, reliable.
python major mode

a merge between the existent python-modes would be a really good move that will simplify everyone’s life. 

## Goodies

### Virtualenv Support

Implementing virtualenv support is a joke, probably a menu for switching them would be a good thing,

### Testing

I’m (gabriele) a believer in testing, so a mode for easily run tests would be a great thing. using the project management utilities and writing a little mode to parse unit test output should be enough to be useful. For example cedet targets can be automatically parsed tests.

### Code Folding

it comes like default on cedet :)

### Cython Mode

Very simple to integrate
RestructuredText support (?)

People like to write in this format.

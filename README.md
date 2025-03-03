[![MELPA](https://melpa.org/packages/python-mode-badge.svg)](https://melpa.org/#/python-mode)

Send source code to all known Python shells without need to reconfigure default.
Provide fine grained navigation of all known Python constructs.

* Initialize
  Make sure the directory, where python-mode.el resides, is in load-path
  For expample put something like that in your init-file: 

  (add-to-list 'load-path "PATH/TO/PYTHON-MODE")\
  (require 'python-mode)\

  or probably still better: 

  (setq py-install-directory "PATH/TO/PYTHON-MODE/")\
  (add-to-list 'load-path py-install-directory)\
  (require 'python-mode)\

* Selecting a Python shell:

  Customize default Python shell as `py-shell-name'

  `py-shell-name' might be an installed default executable as shell
  command `type' would display, but also a PATH/TO/EXECUTABLE

  If different flavours of Python are installed, in order to adress
  them customize var ‘py-known-shells’ and reload. python-mode will
  create the required commands according to contents of this list.

  Depending from your instell the var py-known-shells might show contents like that:
  ("ipython" "ipython2.7" "ipython3" "jython" "python" "python2" "python3" "pypy")

  If needed, customize respective arguments
  py-SHELL-command-args,  i.e.

  py-python-command-args
  py-python3-command-args
  py-ipython-command-args

  etc.

  Commands related to a specific shell start with

  M-x SHELL RET

  This calls py-shell with its default arguments
  With C-u, shell will get an unique name.

  According to contents of ‘py-known-shells’ commands might be
  available like this:

  M-x py-shell RET ==> "*Python*"
  M-x python RET ==> "*Python*"
  M-x ipython RET ==> "*IPython*"
  M-x python3 RET ==> "*Python3*"

  etc.

  A shebang precedes default py-shell-name.
  In case you will run code by default-shell, ignoring shebang, set
  `py-force-py-shell-name-p' to. This might be done also via menu
  Python/.../Switches

  Run a local shell by evaluating

  (defun MY-LOCAL-SHELL ()
  (interactive)
  (py-shell nil DEDICATED PATH-TO-LOCAL-SHELL))

* Execution code
  Python code might be processed by an

- interactive Python shell (DEFAULT)
- non-interactive Python (`py-fast-process-p')
  The latter, while design for large output, seems more reliable - see  also PROBLEMS.org.

Both processes might run in 
- session, i.e. start from possible previous state (DEFAULT)
- dedicated, (`py-dedicated-process-p') run in separate process

There is also
- python-mode-v5-behavior

* Indentation rules
  See customizable variable ‘py-indent-offset’ for default indent
  
  Beside syntactic indent, a couple of conventions should enhance readability.
  
  Customizable variable ‘py-closing-list-dedents-bos’: 
  When non-nil, indent lists closing delimiter like start-column.
  
  my_list = [
    1, 2, 3,
    4, 5, 6
  ]
  
  otherwise

  my_list = [
    1, 2, 3,
    4, 5, 6
    ]

  With nested dicts: 
  
  "# closing, py-closing-list-dedents-bos t
  asdf = {
      'a':{
           'b':3,
           'c':4
      }
  }

  otherwise
  # hanging, py-closing-list-dedents-bos nil
  asdf = {
      'a':{
           'b':3,
           'c':4
          }
      }

  # With opener at EOL, next line indents acording to ‘py-indent-offset’
  def long_function_name(
      var_one, var_two, var_three,
      var_four):
      print(var_one)

  # ‘one-level-from-first-element’ adds ‘py-indent-offset’ from first element
  def foo():
      if (foo &&
              baz):
          bar()"

* Checks
  Access is provided to a couple of known checkers like Flake8, pep8, pylint
  Need to be installed for example calling "pip install pep8" from a shell command-line
  Get flycheck from https://www.flycheck.org

* Displaying Output

  `py-execute-...'-commands arrive in buffer created by
  `py-shell'. It's name is composed WRT to Python
  version used, it's path etc. 

  Result of commands ending  "-fast"
  arrives in `py-fast-output-buffer'
  
* Window management
  Variables in question:

  ** py-keep-windows-configuration 

  Default is nil.
  When non-nil, it takes precedence over
  `py-split-window-on-execute' and `py-switch-buffers-on-execute-p'
  settings

  Useful, if a pre-designed set of python-shells/buffers should be
  maintained

  ** py-split-window-on-execute

  When non-nil split windows according to value set.

  Default is 'just-two: when code is send to interpreter, split screen
  into source-code buffer and current py-shell result. Other buffers
  will be hidden that way.

  When set to `t', python-mode tries to reuse existing windows and
  will split only if needed.

  With 'always, results will displayed in a new window.

  ** py-switch-buffers-on-execute-p

  Put focus into the output buffer: this will display buffer in any
  case

* Completion
  At the end of a word TAB by default calls completion.

  Auto-completion should be available via

  (require 'auto-complete-config)
  (ac-config-default)

  or for company:

  (autoload 'company-mode "company" nil t)

  There is also a Pymacs based auto-completion mode
  see README-PYMACS.org

  Either use Pymacs and `company-mode' with `pycomplete.el' etc. --load
  stuff from folder completion--

  or switch `py-auto-completion-mode-p' - which relies on
  `py-shell-complete', i.e. calls the current Python process.

  Extern tools like jedi-server/mode should work too.


* Moving

  Beside common moves like `defun', `statement', block
  called via py-end-of-..., py-beginning-...
  specific Python-mode edits are delivered:

  `py-expression' and `py-partial-expression'. 

  Statement below is considered composed of two `py-expression' 

  a = ['spam', 'eggs', 100, 1234]
  ||  |_________________________|

  Assigment operator and all inside comments is ignored.

  `py-partial-expression' would match six sections

  a = ['spam', 'eggs', 100, 1234]
  ||   |_____| |_____| |__| |___|
  |_____________________________|

  When traversing code, `py-partial-expression' climbs down and up
  all levels encountered, i.e. at opening `[' `py-expression' would return ['spam', 'eggs', 100, 1234], while one char behind at `''
  it yields `'spam','

- py-sexp-function, 
  When set, it's value is called instead of `forward-sexp', `backward-sexp
  Choices are py-partial-expression, py-expression, default nil


* Filling
  Customize boolean `py-set-fill-column-p'

  If `t', enables use Python specific `fill-column' according to

  `py-docstring-fill-column', default is 72

  and `py-comment-fill-column, default is 79

  Comment- and docstring settings might be disabled by
  any non-integer value, which means: do not use a
  different value of `fill-column' than emacs-wide

* Prefix conventions

  Most python-mode.el commands start with prefix `py-'

  `M-x py- TAB'
  displays a list of them in completion-buffer.
  See also commands list delivered in directory doc.

  List virtualenv related `M-x virtualenv- TAB'
  resp. Pymacs commands `M-x pymacs-'

* Python and IPython

  Start IPython shell after loading python-mode via M-x
  ipython, not from plain shell.

  Executing code through IPython should work as with
  regular Python, also getting completions from. However,
  with IPython, it feels a demi-second slower.

* Session mode
  Py-shell runs in session mode by default. Not to run in session
  mode, customize ‘py-session-p’ to nil. Or for current session call
  ‘py-toggle-session-p’. 

* Troubleshooting

  Start with Emacs -Q from the directory where python-mode.el lives.
  Open python-mode.el and evaluate it.

  Open a file with ending ".py".

  M-x python RET

  a regular Python-shell should appear

  M-x IPython RET

  an IPython-shell should be opened

  ** pdb doesn't work at Windows
  Richard Stanton commented:

  Running M-x pdb doesn't work on my Windows machine, primarily because
  Windows (at least using the default shell) doesn't automatically know
  what to do when you give it a .py command at the command line.

  For example, here's the suggested command when I run pdb on a file
  c:\projects/run.py:

  c:/python27/Lib/pdb.py run.py

  If I accept this, I get an error "Spawning child process: Invalid
  argument"

  A work-around to get it to work is to replace the suggested command
  with

  c:\python27\python -i c:/python27/Lib/pdb.py c:/projects/run.py

  (note that I not only have to add the python command, but also fully
  qualify the script file, since otherwise it complains it can't find
  the file).

* Testing

  File ‘run-tests.sh’ runs tests in batch-mode locally and remote.
  If the shell-variable WERKSTATT is set to 0, local run is assumed.
  The script does
  WERKSTATT=${WERKSTATT:=1}
  i.e. if WERKSTATT is not set, it  will be 1, which means "remote" here.

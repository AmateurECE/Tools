;;; python.el --- Python's flying circus support for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2003-2017 Free Software Foundation, Inc.

;; Author: Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/fgallina/python.el
;; Version: 0.25.2
;; Package-Requires: ((emacs "24.1") (cl-lib "1.0"))
;; Maintainer: emacs-devel@gnu.org
;; Created: Jul 2010
;; Keywords: languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Python files with some fontification and
;; indentation bits extracted from original Dave Love's python.el
;; found in GNU/Emacs.

;; Implements Syntax highlighting, Indentation, Movement, Shell
;; interaction, Shell completion, Shell virtualenv support, Shell
;; package support, Shell syntax highlighting, Pdb tracking, Symbol
;; completion, Skeletons, FFAP, Code Check, Eldoc, Imenu.

;; Syntax highlighting: Fontification of code is provided and supports
;; python's triple quoted strings properly.

;; Indentation: Automatic indentation with indentation cycling is
;; provided, it allows you to navigate different available levels of
;; indentation by hitting <tab> several times.  Also electric-indent-mode
;; is supported such that when inserting a colon the current line is
;; dedented automatically if needed.

;; Movement: `beginning-of-defun' and `end-of-defun' functions are
;; properly implemented.  There are also specialized
;; `forward-sentence' and `backward-sentence' replacements called
;; `python-nav-forward-block', `python-nav-backward-block'
;; respectively which navigate between beginning of blocks of code.
;; Extra functions `python-nav-forward-statement',
;; `python-nav-backward-statement',
;; `python-nav-beginning-of-statement', `python-nav-end-of-statement',
;; `python-nav-beginning-of-block', `python-nav-end-of-block' and
;; `python-nav-if-name-main' are included but no bound to any key.  At
;; last but not least the specialized `python-nav-forward-sexp' allows
;; easy navigation between code blocks.  If you prefer `cc-mode'-like
;; `forward-sexp' movement, setting `forward-sexp-function' to nil is
;; enough, You can do that using the `python-mode-hook':

;; (add-hook 'python-mode-hook
;;           (lambda () (setq forward-sexp-function nil)))

;; Shell interaction: is provided and allows opening Python shells
;; inside Emacs and executing any block of code of your current buffer
;; in that inferior Python process.

;; Besides that only the standard CPython (2.x and 3.x) shell and
;; IPython are officially supported out of the box, the interaction
;; should support any other readline based Python shells as well
;; (e.g. Jython and PyPy have been reported to work).  You can change
;; your default interpreter and commandline arguments by setting the
;; `python-shell-interpreter' and `python-shell-interpreter-args'
;; variables.  This example enables IPython globally:

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i")

;; Using the "console" subcommand to start IPython in server-client
;; mode is known to fail intermittently due a bug on IPython itself
;; (see URL `https://debbugs.gnu.org/cgi/bugreport.cgi?bug=18052#27').
;; There seems to be a race condition in the IPython server (A.K.A
;; kernel) when code is sent while it is still initializing, sometimes
;; causing the shell to get stalled.  With that said, if an IPython
;; kernel is already running, "console --existing" seems to work fine.

;; Running IPython on Windows needs more tweaking.  The way you should
;; set `python-shell-interpreter' and `python-shell-interpreter-args'
;; is as follows (of course you need to modify the paths according to
;; your system):

;; (setq python-shell-interpreter "C:\\Python27\\python.exe"
;;       python-shell-interpreter-args
;;       "-i C:\\Python27\\Scripts\\ipython-script.py")

;; Missing or delayed output used to happen due to differences between
;; Operating Systems' pipe buffering (e.g. CPython 3.3.4 in Windows 7.
;; See URL `https://debbugs.gnu.org/cgi/bugreport.cgi?bug=17304').  To
;; avoid this, the `python-shell-unbuffered' defaults to non-nil and
;; controls whether `python-shell-calculate-process-environment'
;; should set the "PYTHONUNBUFFERED" environment variable on startup:
;; See URL `https://docs.python.org/3/using/cmdline.html#cmdoption-u'.

;; The interaction relies upon having prompts for input (e.g. ">>> "
;; and "... " in standard Python shell) and output (e.g. "Out[1]: " in
;; IPython) detected properly.  Failing that Emacs may hang but, in
;; the case that happens, you can recover with \\[keyboard-quit].  To
;; avoid this issue, a two-step prompt autodetection mechanism is
;; provided: the first step is manual and consists of a collection of
;; regular expressions matching common prompts for Python shells
;; stored in `python-shell-prompt-input-regexps' and
;; `python-shell-prompt-output-regexps', and dir-local friendly vars
;; `python-shell-prompt-regexp', `python-shell-prompt-block-regexp',
;; `python-shell-prompt-output-regexp' which are appended to the
;; former automatically when a shell spawns; the second step is
;; automatic and depends on the `python-shell-prompt-detect' helper
;; function.  See its docstring for details on global variables that
;; modify its behavior.

;; Shell completion: hitting tab will try to complete the current
;; word.  The two built-in mechanisms depend on Python's readline
;; module: the "native" completion is tried first and is activated
;; when `python-shell-completion-native-enable' is non-nil, the
;; current `python-shell-interpreter' is not a member of the
;; `python-shell-completion-native-disabled-interpreters' variable and
;; `python-shell-completion-native-setup' succeeds; the "fallback" or
;; "legacy" mechanism works by executing Python code in the background
;; and enables auto-completion for shells that do not support
;; receiving escape sequences (with some limitations, i.e. completion
;; in blocks does not work).  The code executed for the "fallback"
;; completion can be found in `python-shell-completion-setup-code' and
;; `python-shell-completion-string-code' variables.  Their default
;; values enable completion for both CPython and IPython, and probably
;; any readline based shell (it's known to work with PyPy).  If your
;; Python installation lacks readline (like CPython for Windows),
;; installing pyreadline (URL `http://ipython.org/pyreadline.html')
;; should suffice.  To troubleshoot why you are not getting any
;; completions, you can try the following in your Python shell:

;; >>> import readline, rlcompleter

;; If you see an error, then you need to either install pyreadline or
;; setup custom code that avoids that dependency.

;; Shell virtualenv support: The shell also contains support for
;; virtualenvs and other special environment modifications thanks to
;; `python-shell-process-environment' and `python-shell-exec-path'.
;; These two variables allows you to modify execution paths and
;; environment variables to make easy for you to setup virtualenv rules
;; or behavior modifications when running shells.  Here is an example
;; of how to make shell processes to be run using the /path/to/env/
;; virtualenv:

;; (setq python-shell-process-environment
;;       (list
;;        (format "PATH=%s" (mapconcat
;;                           'identity
;;                           (reverse
;;                            (cons (getenv "PATH")
;;                                  '("/path/to/env/bin/")))
;;                           ":"))
;;        "VIRTUAL_ENV=/path/to/env/"))
;; (python-shell-exec-path . ("/path/to/env/bin/"))

;; Since the above is cumbersome and can be programmatically
;; calculated, the variable `python-shell-virtualenv-root' is
;; provided.  When this variable is set with the path of the
;; virtualenv to use, `process-environment' and `exec-path' get proper
;; values in order to run shells inside the specified virtualenv.  So
;; the following will achieve the same as the previous example:

;; (setq python-shell-virtualenv-root "/path/to/env/")

;; Also the `python-shell-extra-pythonpaths' variable have been
;; introduced as simple way of adding paths to the PYTHONPATH without
;; affecting existing values.

;; Shell package support: you can enable a package in the current
;; shell so that relative imports work properly using the
;; `python-shell-package-enable' command.

;; Shell remote support: remote Python shells are started with the
;; correct environment for files opened remotely through tramp, also
;; respecting dir-local variables provided `enable-remote-dir-locals'
;; is non-nil.  The logic for this is transparently handled by the
;; `python-shell-with-environment' macro.

;; Shell syntax highlighting: when enabled current input in shell is
;; highlighted.  The variable `python-shell-font-lock-enable' controls
;; activation of this feature globally when shells are started.
;; Activation/deactivation can be also controlled on the fly via the
;; `python-shell-font-lock-toggle' command.

;; Pdb tracking: when you execute a block of code that contains some
;; call to pdb (or ipdb) it will prompt the block of code and will
;; follow the execution of pdb marking the current line with an arrow.

;; Symbol completion: you can complete the symbol at point.  It uses
;; the shell completion in background so you should run
;; `python-shell-send-buffer' from time to time to get better results.

;; Skeletons: skeletons are provided for simple inserting of things like class,
;; def, for, import, if, try, and while.  These skeletons are
;; integrated with abbrev.  If you have `abbrev-mode' activated and
;; `python-skeleton-autoinsert' is set to t, then whenever you type
;; the name of any of those defined and hit SPC, they will be
;; automatically expanded.  As an alternative you can use the defined
;; skeleton commands: `python-skeleton-<foo>'.

;; FFAP: You can find the filename for a given module when using ffap
;; out of the box.  This feature needs an inferior python shell
;; running.

;; Code check: Check the current file for errors with `python-check'
;; using the program defined in `python-check-command'.

;; Eldoc: returns documentation for object at point by using the
;; inferior python subprocess to inspect its documentation.  As you
;; might guessed you should run `python-shell-send-buffer' from time
;; to time to get better results too.

;; Imenu: There are two index building functions to be used as
;; `imenu-create-index-function': `python-imenu-create-index' (the
;; default one, builds the alist in form of a tree) and
;; `python-imenu-create-flat-index'.  See also
;; `python-imenu-format-item-label-function',
;; `python-imenu-format-parent-item-label-function',
;; `python-imenu-format-parent-item-jump-label-function' variables for
;; changing the way labels are formatted in the tree version.

;; If you used python-mode.el you may miss auto-indentation when
;; inserting newlines.  To achieve the same behavior you have two
;; options:

;; 1) Enable the minor-mode `electric-indent-mode' (enabled by
;;    default) and use RET.  If this mode is disabled use
;;    `newline-and-indent', bound to C-j.

;; 2) Add the following hook in your .emacs:

;; (add-hook 'python-mode-hook
;;   #'(lambda ()
;;       (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; I'd recommend the first one since you'll get the same behavior for
;; all modes out-of-the-box.

;;; Installation:

;; Add this to your .emacs:

;; (add-to-list 'load-path "/folder/containing/file")
;; (require 'python)

;;; TODO:

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'comint)
(require 'json)
(require 'tramp-sh)

;; Avoid compiler warnings
(defvar view-return-to-alist)
(defvar compilation-error-regexp-alist)
(defvar outline-heading-end-regexp)

(autoload 'comint-mode "comint")
(autoload 'help-function-arglist "help-fns")

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.ptest\\'") 'python-mode))

;;; Python specialized rx

(eval-and-compile
  (defconst python-rx-constituents
    `((block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with"
                                       ;; Python 3.5+ PEP492
                                       (and "async" (+ space)
                                            (or "def" "for" "with")))
                                   symbol-end))
      (dedenter            . ,(rx symbol-start
                                   (or "elif" "else" "except" "finally")
                                   symbol-end))
      (block-ender         . ,(rx symbol-start
                                  (or
                                   "break" "continue" "pass" "raise" "return")
                                  symbol-end))
      (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
      (defun                . ,(rx symbol-start
                                   (or "def" "class"
                                       ;; Python 3.5+ PEP492
                                       (and "async" (+ space) "def"))
                                   symbol-end))
      (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                   (+ space) "==" (+ space)
                                   (any ?' ?\") "__main__" (any ?' ?\")
                                   (* space) ?:))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
      (open-paren           . ,(rx (or "{" "[" "(")))
      (close-paren          . ,(rx (or "}" "]" ")")))
      (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
      ;; FIXME: rx should support (not simple-operator).
      (not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
      ;; FIXME: Use regexp-opt.
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''")))))
      (coding-cookie . ,(rx line-start ?# (* space)
                            (or
                             ;; # coding=<encoding name>
                             (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
                             ;; # -*- coding: <encoding name> -*-
                             (: "-*-" (* space) "coding:" (* space)
                                (group-n 1 (+ (or word ?-))) (* space) "-*-")
                             ;; # vim: set fileencoding=<encoding name> :
                             (: "vim:" (* space) "set" (+ space)
                                "fileencoding" (* space) ?= (* space)
                                (group-n 1 (+ (or word ?-))) (* space) ":")))))
    "Additional Python specific sexps for `python-rx'")

  (defmacro python-rx (&rest regexps)
    "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
    (let ((rx-constituents (append python-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))


;;; Font-lock and syntax

(eval-and-compile
  (defun python-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (`'comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (`'string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (`'paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))

(defun python-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro python-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      (`comment (and (nth 4 ppss) (nth 8 ppss)))
      (`string (and (nth 3 ppss) (nth 8 ppss)))
      (`paren (nth 1 ppss))
      (_ nil))))

(defun python-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst python-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside comment or string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst python-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (eql (syntax-class (syntax-after (point)))
       (syntax-class (string-to-syntax ")"))))

(define-obsolete-function-alias
  'python-info-ppss-context #'python-syntax-context "24.3")

(define-obsolete-function-alias
  'python-info-ppss-context-type #'python-syntax-context-type "24.3")

(define-obsolete-function-alias
  'python-info-ppss-comment-or-string-p
  #'python-syntax-comment-or-string-p "24.3")

(defun python-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      (if (python-info-docstring-p state)
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(defvar python-font-lock-keywords
  ;; Keywords
  `(,(rx symbol-start
         (or
          "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
          "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
          "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
          "try"
          ;; Python 2:
          "print" "exec"
          ;; Python 3:
          ;; False, None, and True are listed as keywords on the Python 3
          ;; documentation, but since they also qualify as constants they are
          ;; fontified like that in order to keep font-lock consistent between
          ;; Python versions.
          "nonlocal"
          ;; Python 3.5+ PEP492
          (and "async" (+ space) (or "def" "for" "with"))
          "await"
          ;; Extra:
          "self")
         symbol-end)
    ;; functions
    (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    ;; classes
    (,(rx symbol-start "class" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    ;; Constants
    (,(rx symbol-start
          (or
           "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__"
           ;; copyright, license, credits, quit and exit are added by the site
           ;; module and they are not intended to be used in programs
           "copyright" "credits" "exit" "license" "quit")
          symbol-end) . font-lock-constant-face)
    ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; Builtin Exceptions
    (,(rx symbol-start
          (or
           ;; Python 2 and 3:
           "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
           "BufferError" "BytesWarning" "DeprecationWarning" "EOFError"
           "EnvironmentError" "Exception" "FloatingPointError" "FutureWarning"
           "GeneratorExit" "IOError" "ImportError" "ImportWarning"
           "IndentationError" "IndexError" "KeyError" "KeyboardInterrupt"
           "LookupError" "MemoryError" "NameError" "NotImplementedError"
           "OSError" "OverflowError" "PendingDeprecationWarning"
           "ReferenceError" "RuntimeError" "RuntimeWarning" "StopIteration"
           "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit" "TabError"
           "TypeError" "UnboundLocalError" "UnicodeDecodeError"
           "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError"
           "UnicodeWarning" "UserWarning" "ValueError" "Warning"
           "ZeroDivisionError"
           ;; Python 2:
           "StandardError"
           ;; Python 3:
           "BlockingIOError" "BrokenPipeError" "ChildProcessError"
           "ConnectionAbortedError" "ConnectionError" "ConnectionRefusedError"
           "ConnectionResetError" "FileExistsError" "FileNotFoundError"
           "InterruptedError" "IsADirectoryError" "NotADirectoryError"
           "PermissionError" "ProcessLookupError" "RecursionError"
           "ResourceWarning" "StopAsyncIteration" "TimeoutError"
           ;; OS specific
           "VMSError" "WindowsError"
           )
          symbol-end) . font-lock-type-face)
    ;; Builtins
    (,(rx symbol-start
          (or
           "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
           "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
           "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
           "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
           "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
           "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
           "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
           "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
           "__import__"
           ;; Python 2:
           "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
           "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
           "intern"
           ;; Python 3:
           "ascii" "bytearray" "bytes" "exec"
           ;; Extra:
           "__all__" "__doc__" "__name__" "__package__")
          symbol-end) . font-lock-builtin-face)
    ;; assignments
    ;; support for a = b = c = 5
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_)))
                             (? ?\[ (+ (not (any  ?\]))) ?\]) (* space)
                             assignment-operator))
              (res nil))
          (while (and (setq res (re-search-forward re limit t))
                      (or (python-syntax-context 'paren)
                          (equal (char-after (point)) ?=))))
          res))
     (1 font-lock-variable-name-face nil nil))
    ;; support for a, b, c = (1, 2, 3)
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                             (* ?, (* space) (+ (any word ?. ?_)) (* space))
                             ?, (* space) (+ (any word ?. ?_)) (* space)
                             assignment-operator))
              (res nil))
          (while (and (setq res (re-search-forward re limit t))
                      (goto-char (match-end 1))
                      (python-syntax-context 'paren)))
          res))
     (1 font-lock-variable-name-face nil nil))))

(defconst python-syntax-propertize-function
  (syntax-propertize-rules
   ((python-rx string-delimiter)
    (0 (ignore (python-syntax-stringify))))))

(defconst python--prettify-symbols-alist
  '(("lambda"  . ?λ)
    ("and" . ?∧)
    ("or" . ?∨)))

(defsubst python-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun python-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (python-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

(defvar python-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Python files.")

(defvar python-dotty-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Python files.
It makes underscores and dots word constituent chars.")

;;; Utility functions


;;;###autoload
(define-derived-mode python-mode prog-mode "Python (Test)"
  "Major mode for editing Python files.

\\{python-mode-map}"
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (set (make-local-variable 'forward-sexp-function)
       'python-nav-forward-sexp)

  (set (make-local-variable 'font-lock-defaults)
       '(python-font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-face-function
          . python-font-lock-syntactic-face-function)))

  (set (make-local-variable 'syntax-propertize-function)
       python-syntax-propertize-function)
)

(provide 'python)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; python.el ends here

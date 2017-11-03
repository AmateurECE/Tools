;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    matlab-mode.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    This was my attempt at developing a matlab major mode for
;;		    emacs. It was working, too, until I discovered matlab.el,
;;		    which is actually provided by Mathworks. Theirs has more
;;		    sugar than mine, but they both have about the same syntactic
;;		    functionality. However, since their programmers were paid to
;;		    do it, I can only assume their is more sturdy. Thus, I'll be
;;		    changing to use theirs until further notice, but I'll keep
;;		    this file around for reference for the time being. I'll be
;;		    changing all of the 'TODO' statmenets to 'NODO' so that they
;;		    do not clutter up my space.
;;
;; CREATED:	    06/28/2017
;;
;; LAST EDITED:	    11/03/2017
;;;

;; NODO: Fix indentation for lines that continue with '...'
;;	    This doesn't work inside of a ['"] delimited string.
;; NODO: Create a face for code section titles? %%...%%
;;	    (Regular comment face...but BOLDER)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INCLUSIONS
;;;

(require 'rx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONTIFICATION
;;;

(eval-and-compile
  (defconst matlab-rx-constituents
    `((word-symbol	    . ,(rx (any word ?_)))

      (comparison-operator  . ,(rx (or "==" ">=" "<=" "~=" "<" ">")))

      (assignment-operator  . ,(rx (and (or ?=)
					  (not (any ?> ?< ?= ?~)))
					  ))

      (matlab-keywords	    . ,(regexp-opt '(;; Keywords in MATLAB
					     "break" "case" "catch"
					     "dbcont" "else" "elseif"
					     "end" "for" "global" "if"
					     "otherwise" "persistent"
					     "return" "switch" "try"
					     "while" "function")
					   t))

      (begin-block	    . ,(regexp-opt '(;; Keywords that being a block
					     "switch" "try" "while"
					     "case" "catch" "if"
					     "elseif" "else" "for")
					   t))
      (mid-block	    . ,(regexp-opt '(;; How do I explain these?
					     "elseif" "case" "else" "catch")
					   t))
      (end-block	    . ,(regexp-opt '("end")
					   t))
      "Custom `rx' constituents for matlab-rx macro"))

  (defmacro matlab-rx (&rest regexps)
    "Custom `rx' macro for MATLAB mode"
    (let ((rx-constituents (append matlab-rx-constituents rx-constituents)))
      (cond ((null regexps)
	     (error "No regexp"))
	    ((cdr regexps)
	     (rx-to-string `(and ,@regexps) t))
	    (t
	     (rx-to-string (car regexps) t)))))

  (defvar matlab-font-lock-keywords 
    `(;; Keywords
      (,(matlab-rx symbol-start
	    matlab-keywords
	    symbol-end)
       . font-lock-keyword-face)
      ;; Assignments (of the form a = b; a(i) = b)
      (,(matlab-rx (group (+ word)) (* space)
		   (? ?\( (* (not (any ?\)))) ?\)) (* space)
		   assignment-operator)
       (1 font-lock-variable-name-face nil nil)
       (matlab-fontify-comment-block
	(1 font-lock-comment-face))
       (matlab-find-block-comments
	(1 font-lock-comment-face prepend) ; commented out
	(2 'underline prepend)
	(3 'underline prepend);the comment parts
	     )))
    "Minimal highlighting expressions for MATLAB mode"))

; Create the syntax table
(defvar matlab-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ? ">" st)
    st)
  "Syntax table for MATLAB Mode")

(defun matlab-find-block-comments (limit)
    "Find code that is commented out with %{ until %}.
Argument LIMIT is the maximum distance to search."
    (if (and (< (point) limit)
	     (re-search-forward "%{" limit t))
	(let ((b1 (match-beginning 0))
	      (e1 (match-end 0))
	      (b2 nil) (e2 nil)
	      (b3 nil) (e3 nil))
	  (goto-char b1)
	  (forward-char -1)
	  (when (not (matlab-cursor-in-comment))
	    (setq b2 (re-search-forward "%}" limit t))
	    (when b2
	      (setq b2 (match-beginning 0)
		    e2 (match-end 0))
	      (set-match-data
	       (list b1 e2  ; full match
		     b1 e2  ; the full comment
		     b1 e1  ; the block start
		     b2 e2  ; the block end
		     ))
	      t
	          )))))

(defun matlab-fontify-comment-block (lim)
  "Returns the position of any block comments to be fontified."
  (save-excursion
    (let ((lim (if lim lim (point-max)))
	  beg end)
      (setq beg (re-search-forward "%{" limit t))
      (if (not (= beg (point)))
	  (progn
	    (goto-char beg)
	    (if (re-search-forward "%}" limit 'kp)
		`(beg ,(point))
	      nil))
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDENTATION
;;;

(defvar matlab-current-indent 0
  "Current column of indentation in MATLAB")

(defvar matlab-previous-indent 0
  "Indentation column of previous line in MATLAB mode.")

(defcustom matlab-default-tab-width 4
  "Default tab stop width in `matlab-mode'"
  :type 'int
  :group 'matlab)

(defun matlab-previous-indent ()
  "Calculate the indent of the previous line."
  (save-excursion
    (previous-logical-line)
    (beginning-of-line)
    (let ((oldpos (point)))
      (skip-chars-forward " \t")
      (/ (- (point) oldpos) matlab-default-tab-width))))

(defun matlab-calculate-indent ()
  "Return the indentation for the current line."
  ;; Calculate the indent:
  ;; 1. Calculate indent of previous line.
  ;; 2. Calculate indent: (p - previous line; c - current line)
  ;;	* p/begin-block/	    (+ ... 1)
  ;;	* c/mid-block|end-block/    (- ... 1)
  ;;	* p/end-block/		    (+ ... 0)
  (if (bobp)
      0
    (save-excursion
      (let ((prev-indent (* (matlab-previous-indent) matlab-default-tab-width))
	    (oldpos (point))
	    indent eol)
	(previous-logical-line) ;; Search previous line first
	(beginning-of-line)
	(setq eol (matlab-end-of-line))
	(cond
	 ;; p/begin-block/
	 ;; 'kp - forces re-search-forward to keep point position after search.
	 ((and (re-search-forward (matlab-rx begin-block) eol 'kp)
	       (not (re-search-forward (matlab-rx end-block) eol 'kp)))
	  (if (not (matlab-in-comment-p))
	      (setq indent (+ prev-indent matlab-default-tab-width))
	    (setq indent prev-indent)))
	 ;; p/end-block/
	 ((re-search-forward (matlab-rx end-block) eol 'kp)
	  (setq indent prev-indent))
	 ;; Default
	 (t (setq indent prev-indent)))
	(goto-char oldpos)
	(beginning-of-line)
	(setq eol (matlab-end-of-line))
	(if (and indent
		 (re-search-forward
		  (matlab-rx (or mid-block end-block)) eol 'kp))
	    (if (not (matlab-in-comment-p))
		(setq indent (- indent matlab-default-tab-width))
	      (setq indent prev-indent)))
	(matlab-debug
	 (format "Indent: %d; Previous indent: %d" indent prev-indent))
	(if (< indent 0)
	    0
	  indent)))))

(defun matlab-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((indent (matlab-calculate-indent))
	(pos (- (point-max) (point))))
    (when indent
      (beginning-of-line)
      (skip-chars-forward " \t")
      (indent-line-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES
;;;

(defun matlab-in-comment-p ()
  "Returns non-nil if point is inside of a comment"
  (let ((face (remq nil `(,(get-char-property (point) 'read-face-name)
			  ,(get-char-property (point) 'face)
			  ,(plist-get (text-properties-at (point)) 'face)))))
    (if (or (memq 'font-lock-comment-face face)
	    (memq 'font-lock-comment-delimiter-face face))
	t
      nil)))

(defun matlab-end-of-line ()
  "Return the position of the end of the line."
  (save-excursion
    (end-of-line)
    (point)))

(defcustom matlab-debug-statements nil
  "If this is set to t, functions in `matlab-mode' will print debugging
messages to the `*Messages*' buffer"
  :type 'boolean
  :group 'matlab)

(defmacro matlab-debug (&rest stmts)
  (if matlab-debug-statements
      (message stmts))
  "Prints debugging messages, if `matlab-debug-statements' is true.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE SETUP
;;;

(defgroup matlab nil "The Group for MATLAB customizeable variables.")

(defvar matlab-mode-hook nil)

(defvar matlab-mode-map
  (let ((map (make-keymap)))
    map)
  "Key map for MATLAB Major mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    matlab-mode
;;
;; DESCRIPTION:	    Entry function for matlab-mode. This function is called
;;		    upon discovery of a buffer that is named *.m. It sets all of
;;		    the rules and variables and sets up the environment.
;;
;; ARGUMENTS:	    none
;;
;; RETURN:	    void.
;;
;; NOTES:	    Entry function.
;;;
(define-derived-mode matlab-mode prog-mode "MATLAB" ()
  "Major-mode for editing MATLAB Code."
  (set-syntax-table matlab-mode-syntax-table)
  (use-local-map matlab-mode-map)

  ;; Fontification
  (set (make-local-variable 'font-lock-defaults)
       '(matlab-font-lock-keywords
	 nil nil nil))
  
  ;; Indentation
  (set (make-local-variable 'indent-line-function) #'matlab-indent-line)
  
  (setq major-mode 'matlab-mode)
  (run-hooks 'matlab-mode-hook)
)

;; Set the file extension that triggers this mode.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

(provide 'matlab-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    matlab-mode.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    A Major mode for editing MATLAB code in emacs. That's right.
;;
;; CREATED:	    06/28/2017
;;
;; LAST EDITED:	    06/28/2017
;;;

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
      (matlab-keywords	    . ,(regexp-opt '(
					     "break" "case" "catch"
					     "dbcont" "else" "elseif"
					     "end" "for" "global" "if"
					     "otherwise" "persistent"
					     "return" "switch" "try"
					     "while" "function") t))
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
    `(
      ;; Keywords
      (,(matlab-rx symbol-start
	    matlab-keywords
	    symbol-end)
       . font-lock-keyword-face)
      ;; Assignments (of the form a = b; a(i) = b)
      (,(matlab-rx (group (+ word)) (* space)
		   (? ?\( (* (not (any ?\)))) ?\)) (* space)
		   assignment-operator)
       (1 font-lock-variable-name-face nil nil))
      )
    "Minimal highlighting expressions for MATLAB mode"))

; Create the syntax table
(defvar matlab-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for MATLAB Mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDENTATION
;;;

;; Indentation rules for MATLAB
;;  Rule 1: If we are at the beginning of the buffer, set indent to 0;
;;  Rule 2: If we are currently at "end" de-indent relative to the prev line.
;;  Rule 3: If we see an "end" before current line, indent to the "end" line.
;;  Rule 4: If we see a start line (for, if, while, etc.), increase indentation.
;;  Rule 5: If none of the above apply, do not indent at all.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    matlab-indent-line
;;
;; DESCRIPTION:	    Creates the indentation scheme for the matlab major mode.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    void.
;;
;; NOTES:	    See above for indentation rules.
;;;
(defun matlab-indent-line ()
  "Indent the current line as MATLAB code."
  (interactive)
  (beginning-of-line)

  (if (bobp)
      (indent-line-to-0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*end")
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) default-tab-width))
	      )
	    (if (< cur-indent 0)
		(setq cur-indent 0)
	      )
	    )
	(save-excursion
	  (while not-indented
	    (forward-line -1)
	    (if (looking-at "^[ \t]*end")
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at `(concat "^[ \t]*"
				       "\\(?:ca\\(?:se\\|tch\\)"
				       "\\|else\\(?:if\\)?"
				       "\\|for"
				       "\\|if"
				       "\\|otherwise"
				       "\\|switch"
				       "\\|try"
				       "\\|while\\)"))
		  (progn
		    (setq cur-indent (current-indentation))
		    (setq not-indented nil)
		    )
		(if (bobp)
		    (setq not-indented nil)
		  ))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to-0)
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE SETUP
;;;

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
  ;; (...)
  
  (setq major-mode 'matlab-mode)
  (run-hooks 'matlab-mode-hook)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURAL STATEMENTS
;;;

;; Set the file extension that triggers this mode.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

(provide 'matlab-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

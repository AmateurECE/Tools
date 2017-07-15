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
;; VARIABLE DECLARATIONS
;;;

;; Initial mode setup

(defvar matlab-mode-hook nil)

(defvar matlab-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-j") 'newline-and-indent)
    map)
  "Key map for MATLAB Major mode")

;; Create the initial set of highlighted keywords
;; Reserved keywords in matlab:
;; break    case    catch   dbcont  else	elseif
;; end	    for	    global  if	    otherwise	persistent
;; return   switch  try	    while   function

;; Note: This constant does not declare special keywords
;; that require auxiliary phrases, such as 'function.'
(defconst matlab-font-lock-keywords-1
  (list 
   `(,(concat "\\<"
		(regexp-opt '("break" "case" "catch"
			      "dbcont" "else" "elseif"
			      "end" "for" "global" "if"
			      "otherwise" "persistent"
			      "return" "switch" "try"
			      "while" "function") t)
		"\\>") . font-lock-builtin-face)
   '("[ \t\n]*\\(\(\\[[:alnum:]_]+\)\\)[ \t]*=" ((match-string 1) . font-lock-variable-name-face))
  "Minimal highlighting expressions for MATLAB mode")

;;(defconst matlab-font-lock-keywords-2
;;  (append matlab-font-lock-keywords-1))

(defvar matlab-font-lock-keywords matlab-font-lock-keywords-1
  "Default Highlighting for MATLAB Keywords")

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
;; FUNCTIONS
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
	)))
  )

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
(defun matlab-mode ()
  "Major-mode for editing MATLAB Code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table matlab-mode-syntax-table)
;  (use-local-map matlab-mode-map))
  (set (make-local-variable 'font-lock-defaults) '(matlab-font-lock-keywords))
;  (set (make-local-variable 'indent-line-function) 'matlab-indent-line)
  (setq major-mode 'matlab-mode)
  (setq mode-name "MATLAB")
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

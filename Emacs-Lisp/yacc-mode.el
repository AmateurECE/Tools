;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    yacc-mode.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    This is just a simple derived mode for editing Yacc/Lex.
;;
;; CREATED:	    09/25/2017
;;
;; LAST EDITED:	    09/25/2017
;;;

(eval-when-compile
  (defvar yacc-special-delimiters-re
    (regexp-opt '("%{" "%}" "%%") t)
    "Special delimiters added by the Yacc parser.")

  (defvar yacc-token-declarators-re
    (regexp-opt '("%union" "%token" "%type" "%option" "%start"
		  "%left" "%right" "%nonassoc" "%code" "%empty")
		t)
    "Token declarators for Yacc scripts.")

  (defvar yacc-font-lock-extra-keywords
    `((,yacc-token-declarators-re . font-lock-keyword-face)
      (,yacc-special-delimiters-re . font-lock-preprocessor-face))
      ;; (,(rx line-start (* space) (+ word) ?:) . font-lock-constant-face)
      ;; ((lambda(limit) (let ((section (yacc-section-p)))
      ;; 		    (cond
      ;; 		     ((or (eq section 'yacc-pre-grammar)
      ;; 			  (eq section 'yacc-grammar))
      ;; 		      (message "This is some stuff")
      ;; 		      (re-search-forward
      ;; 		       (rx (group (* space) (+ word) (*space)) ?:)
      ;; 		       limit 'keep-point)
      ;; 		      t)
      ;; 		     (t t))))
      ;;  (0 font-lock-constant-face)))
    "Extra keywords defined by the Yacc grammar."))

(defvar yacc-current-indentation-level 0
  "Variable containing the current indentation level in the yacc mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;

(defun yacc-section-p ()
  "Determines the section that point is in.
Returns 'yacc-pre-c if point is before the %{ %} C-Declaration section.
Returns 'yacc-c if point is in the %{ %} C-Declaration section.
Returns 'yacc-pre-grammar if point is before the %% %% Yacc Grammar section.
Returns 'yacc-grammar if point is in the %% %% Yacc Grammar section.
Returns 'yacc-post-grammar if point is after the %% %% Yacc Grammar section."
  ;; TODO: Employ point-to-register here
  ;;	-- save-excursion is not enough here. The position of the buffer is
  ;;	changed during the call if the buffer is not wide enough. Fix this
  ;;	by employing point-to-register and jump-to-register. Make sure to
  ;;	include in the documentation that we use a register.
  (save-excursion
    (let ((bob (save-excursion (beginning-of-buffer) (point))))
      (re-search-backward yacc-special-delimiters-re bob 'keep-point)
      (if (bobp)
	  'yacc-pre-c
	(cond
	 ((looking-at "%{") 'yacc-c)
	 ((looking-at "%}") 'yacc-pre-grammar)
	 ((looking-at "%%")
	  (progn
	    (let ((eob (save-excursion (end-of-buffer) (point))))
	      (goto-char (+ (point) 3))
	      ;; 'keep-point is a throwaway value which causes re-search
	      ;; to leave point at the boundary of the region.
	      (re-search-forward "%%" eob 'keep-point)
	      (if (eobp)				
		  'yacc-post-grammar
		'yacc-grammar))))
	 (t nil))))))

(defun yacc-test-section-p ()
  (interactive)
  (let ((section (yacc-section-p)))
    (cond
     ((eq section 'yacc-pre-c)
      (message "Pre-C"))
     ((eq section 'yacc-c)
      (message "C"))
     ((eq section 'yacc-pre-grammar)
      (message "Pre-Grammar"))
     ((eq section 'yacc-grammar)
      (message "Grammar"))
     ((eq section 'yacc-post-grammar)
      (message "Post-Grammar")))))

(defun yacc-make-syntax-propertize-function ()
  "Return a value for `syntax-propertize-function' in Yacc-mode.
Used to fontify the special delimiters %{, %}, and %% with
`font-lock-preprocessor-face' and to associate them with the whitespace syntax
class so that they are not adversely affected by indentation functions."
  ;; TODO: Fontify char class aliases in 'yacc-pre-grammar section
  (eval
   `(syntax-propertize-rules
     (,yacc-special-delimiters-re (0 " ")))))

(defun yacc-get-syntax ()
  "Prints the syntax class to the minibuffer in yacc-mode."
  (interactive)
  (let ((str (syntax-class (syntax-after (point)))))
    (message (format "Syntax Descriptor: %d" str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;

(defun yacc-indent-line-or-region ()
  "Indentation function for yacc-mode. This function will call yacc-indent-line
or yacc-indent-region according to the position of the region."
  (interactive)
  (cond
   ((use-region-p) (yacc-indent-region))
   (t (yacc-indent-line))))

(defun yacc-indent-region ()
  "Indents a marked region of code according to yacc-mode."
  (interactive)
  (message "Indenting region...done")) ;; People have come to expect this.

(defun yacc-indent-line ()
  "Indents a line of code according to yacc-mode."
  ;; TODO: Fix indentation offset in yacc-grammar section
  ;;	-- When indenting in yacc-grammar section, if there is an open bracket
  ;;	on the previous line, indent to c-basic-offset + (column of bracket).
  (interactive)
  (let ((section (yacc-section-p)))
    (cond
     ((or (eq section 'yacc-pre-c)
	  (eq section 'yacc-c)
	  (eq section 'yacc-post-grammar))
      (with-syntax-table c-mode-syntax-table (c-indent-line)))
     ((or (eq section 'yacc-pre-grammar)
	  (eq section 'yacc-grammar))
      (let ((eos (point))
	    (bol (save-excursion (beginning-of-line) (point)))
	    curr-pos)
	(save-excursion
	  (previous-line)
	  (beginning-of-line)
	  ;; TODO: Find a better way to determine indentation level.
	  (while (and (re-search-forward "{" eos 'keep-point)
		      (re-search-forward "}" eos 'keep-point)))
	  (if (looking-at "{")
	      (setq curr-pos (+ (current-column) c-basic-offset))
	    nil)
	  nil)
	(if curr-pos
	    (progn
	      (beginning-of-line)
	      (indent-to curr-pos))
	  nil)))
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;

(define-derived-mode yacc-mode c-mode "Yacc/Lex"
  "This mode is for editing Yacc/Lex Scripts. It attempts to be as simple
as possible, deriving most of its syntactic and fontification elements from
c-mode."

  ;; Syntax table
  ;; NOTE: Use the default c-mode-syntax-table.
  (set (make-local-variable 'syntax-propertize-function)
        (yacc-make-syntax-propertize-function))

  ;; Key bindings
  (define-key yacc-mode-map (kbd "C-i") 'yacc-indent-line-or-region)

  ;; Indentation
  (set (make-local-variable 'indent-line-function) #'yacc-indent-line-or-region)
  (setq c-basic-offset 4)
  
  ;; Font lock
  (font-lock-add-keywords nil yacc-font-lock-extra-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(regexp-opt '(".l" ".y") t) . yacc-mode))

(provide 'yacc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

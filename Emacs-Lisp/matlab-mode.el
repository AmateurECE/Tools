;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    matlab-mode.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    A Major mode for editing MATLAB code in emacs. That's right.
;;
;; CREATED:	    06/28/2017
;;
;; LAST EDITED:	    09/21/2017
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

(defvar matlab-current-indent 0
  "Current column of indentation in MATLAB")

(defcustom matlab-default-tab-width 4
  "Default tab stop width in MATLAB mode")

(defvar matlab-kw-alist '("for" "if" "elseif" "else" "while" "switch" "case"))

(defun matlab-prev-thing ()
  "Return the previous thing this logical line."
  ;; This is called when `matlab-get-indent-info' is working backwards on
  ;; the previous line(s) finding what keywords may be relevant for
  ;; indenting.  It moves over sexps if possible, and will stop
  ;; on a ; and at the beginning of a line if it is not a continuation
  ;; line.
  ;;
  ;; Added a kludge for ";;"
  ;; Possible return values:
  ;;  nil  -  nothing
  ;; a string - possibly a keyword
  ;;
  (if (bolp)
      nil
    (let ((start (point))
          (min-point (if (matlab-this-is-a-continuation)
                         (matlab-prev-line nil)
                       (line-beginning-position))))
      (skip-chars-backward " \t;" min-point)
      (if (looking-at "\\s-*;[;&]")
          ;; (message "Found ;; !")
          ";;"
        (skip-chars-backward "^)}];\"'`({[" min-point)
        (let ((c (if (> (point) min-point) (char-before))))
          (matlab-debug "stopping at %d c is %s  start=%d min-point=%d"
                    (point) c start min-point)
          (if (not (memq c '(?\n nil ?\;)))
              ;; c	-- return a string
              (char-to-string c)
            ;; Return the leading keyword of the "command" we supposedly
            ;; skipped over.  Maybe we skipped too far (e.g. past a `do' or
            ;; `then' that precedes the actual command), so check whether
            ;; we're looking at such a keyword and if so, move back forward.
            (let ((boundary (point))
                  kwd next)
              (while
                  (progn
                    ;; Skip forward over white space newline and \ at eol.
                    (skip-chars-forward " \t\n\\\\" start)
                    (if (>= (point) start)
                        (progn
                          (matlab-debug "point: %d >= start: %d" (point) start)
                          nil)
                      (if next (setq boundary next))
                      (matlab-debug "Now at %d   start=%d" (point) start)
                      (setq kwd (matlab-get-word))
                      (if (member kwd (matlab-feature matlab-leading-keywords))
                          (progn
                            (setq next (point))
                            t)
                        nil))))
              (goto-char boundary)
              kwd)))))))

(defun matlab-prev-line (&optional end)
  "Back to end of previous non-comment non-empty line.
Go to beginning of logical line unless END is non-nil, in which case
we go to the end of the previous line and do not check for continuations."
  (save-excursion
    (beginning-of-line)
    (forward-comment (- (point-max)))
    (unless end (beginning-of-line))
    (when (and (not (bobp))
	       (eq (get-text-property (1- (point)) 'face) 'matlab-heredoc))
      (let ((p1 (previous-single-property-change (1- (point)) 'face)))
	(when p1
	  (goto-char p1)
	  (if end
	      (end-of-line)
	    (beginning-of-line)))))
    (unless end
      ;; we must check previous lines to see if they are continuation lines
      ;; if so, we must return position of first of them
      (while (and (matlab-this-is-a-continuation)
		  (>= 0 (forward-line -1))))
      (beginning-of-line)
      (skip-chars-forward " \t"))
    (point)))

(defun matlab-check-rule (n thing)
  (let ((rule (nth n (assoc thing matlab-kw-alist)))
	(val nil))
    (if rule
	(progn
	  (setq val (funcall rule))
	  (matlab-debug "rule (%d) for %s at %d is %s\n-> returned %s"
		    n thing (point) rule val)))
    val))

(defun matlab-get-kw (&optional where and-move)
  "Return first word of line from WHERE.
If AND-MOVE is non-nil then move to end of word."
  (let ((start (point)))
    (if where
	(goto-char where))
    (prog1
	(buffer-substring (point)
			  (progn (skip-chars-forward "^ \t\n;&|")(point)))
      (unless and-move
	(goto-char start)))))

(defun matlab-this-is-a-continuation ()
  "Return non-nil if current line is a continuation of previous line."
  (save-excursion
    (and (zerop (forward-line -1))
	 (looking-at ".*\\\\$")
	 (not (nth 4 (parse-partial-sexp (match-beginning 0) (match-end 0)
					 nil nil nil t))))))

(defun matlab-get-indent-info ()
  "Return indent-info for this line.
This is a list.  nil means the line is to be left as is.
Otherwise it contains one or more of the following sublists:
\(t NUMBER)   NUMBER is the base location in the buffer that indentation is
	     relative to.  If present, this is always the first of the
	     sublists.  The indentation of the line in question is
	     derived from the indentation of this point, possibly
	     modified by subsequent sublists.
\(+ VAR)
\(- VAR)      Get the value of variable VAR and add to or subtract from
	     the indentation calculated so far.
\(= VAR)      Get the value of variable VAR and *replace* the
	     indentation with its value.  This only occurs for
	     special variables such as `matlab-indent-comment'.
STRING	     This is ignored for the purposes of calculating
	     indentation, it is printed in certain cases to help matlabow
	     what the indentation is based on."
  ;; See comments before `matlab-kw'.
  (save-excursion
    (let ((have-result nil)
	  this-kw
	  val
	  (result nil)
	  (align-point nil)
	  prev-line-end x)
      (beginning-of-line)
      ;; Note: setting result to t means we are done and will return nil.
      ;;(This function never returns just t.)
      (cond
       ((or (nth 3 (syntax-ppss (point)))
	    (eq (get-text-property (point) 'face) 'matlab-heredoc))
	;; String continuation -- don't indent
	(setq result t)
	(setq have-result t))
       ((looking-at "\\s-*#")		; was (equal this-kw "#")
	(if (bobp)
	    (setq result t) ;; return nil if 1st line!
	  (setq result (list '(= matlab-indent-comment)))
	  ;; we still need to get previous line in case
	  ;; matlab-indent-comment is t (indent as normal)
	  (setq align-point (matlab-prev-line nil))
	  (setq have-result nil)
	  ))
       ) ;; cond

      (unless have-result
	;; Continuation lines are handled specially
	(if (matlab-this-is-a-continuation)
	    (progn
              (setq result
                    (if (save-excursion
                          (beginning-of-line)
                          (not (memq (char-before (- (point) 2)) '(?\s ?\t))))
                        ;; By convention, if the continuation \ is not
                        ;; preceded by a SPC or a TAB it means that the line
                        ;; is cut at a place where spaces cannot be freely
                        ;; added/removed.  I.e. do not indent the line.
                        (list '(= nil))
                      ;; We assume the line being continued is already
                      ;; properly indented...
                      ;; (setq prev-line-end (matlab-prev-line))
                      (setq align-point (matlab-prev-line nil))
                      (list '(+ matlab-indent-for-continuation))))
	      (setq have-result t))
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (setq this-kw (matlab-get-kw)))

        ;; Handle "this" keyword:  first word on the line we're
	;; calculating indentation info for.
	(if this-kw
	    (if (setq val (matlab-check-rule 1 this-kw))
		(progn
		  (setq align-point (point))
		  (matlab-debug
		   "this - setting align-point to %d" align-point)
		  (setq result (append result val))
		  (setq have-result t)
		  ;; set prev-line to continue processing remainder
		  ;; of this line as a previous line
		  (setq prev-line-end (point))
		  ))))

      (unless have-result
	(setq prev-line-end (matlab-prev-line 'end)))

      (if prev-line-end
	  (save-excursion
	    ;; We start off at beginning of this line.
	    ;; Scan previous statements while this is <=
	    ;; start of previous line.
	    (goto-char prev-line-end)
	    (setq x t)
	    (while (and x (setq x  (matlab-prev-thing)))
	      (matlab-debug "at %d x is: %s  result is: %s" (point) x result)
	      (cond
	       ((and (equal x ")")
		     (equal (get-text-property (1- (point)) 'syntax-table)
			    matlab-st-punc))
		(matlab-debug "Case label) here")
		(setq x 'case-label)
		(if (setq val (matlab-check-rule 2 x))
		    (progn
		      (setq result (append result val))
		      (setq align-point (point))))
		(or (bobp)
		    (forward-char -1))
                ;; FIXME: This charset looks too much like a regexp.  --Stef
		(skip-chars-forward "[a-z0-9]*?")
		)
	       ((string-match "[])}]" x)
		(setq x (matlab-safe-forward-sexp -1))
		(if x
		    (progn
		      (setq align-point (point))
		      (setq result (append result
					   (list "aligned to opening paren")))
		      )))
	       ((string-match "[[({]" x)
		(matlab-debug "Checking special thing: %s" x)
		(if (setq val (matlab-check-rule 2 x))
		    (setq result (append result val)))
		(forward-char -1)
		(setq align-point (point)))
	       ((string-match "[\"'`]" x)
		(matlab-debug "Skipping back for %s" x)
		;; this was oops-2
		(setq x (matlab-safe-forward-sexp -1)))
	       ((stringp x)
		(matlab-debug "Checking string %s at %s" x (point))
		(if (setq val (matlab-check-rule 2 x))
		    ;; (or (eq t (car val))
		    ;; (eq t (car (car val))))
		    (setq result (append result val)))
		;; not sure about this test Wed Jan 27 23:48:35 1999
		(setq align-point (point))
		(unless (bolp)
		  (forward-char -1)))
	       (t
		(error "Don't know what to do with %s" x))
	       )
	      )	;; while
	    (matlab-debug "result is %s" result)
	    )
	(matlab-debug "No prev line!")
	(matlab-debug "result: %s  align-point: %s" result align-point)
	)

      (if align-point
	  ;; was: (setq result (append result (list (list t align-point))))
	  (setq result (append  (list (list t align-point)) result))
	)
      (matlab-debug "result is now: %s" result)

      (or result
	  (setq result (list (if prev-line-end
                                 (list t prev-line-end)
                               (list '= 'matlab-first-lines-indent)))))

      (if (eq result t)
	  (setq result nil))
      (matlab-debug  "result is: %s" result)
      result
      )	;; let
    ))

(defun matlab-calculate-indent (&optional info)
  "Return the indentation for the current line.
If INFO is supplied it is used, else it is calculated from current line."
  (let ((ofs 0)
	(base-value 0)
	elt a b val)
    (or info
	(setq info (matlab-get-indent-info)))
    (when info
      (while info
	(matlab-debug "info: %s  ofs=%s" info ofs)
	(setq elt (car info))
	(cond
	 ((stringp elt)) ;; do nothing?
	 ((listp elt)
	  (setq a (car (car info)))
	  (setq b (nth 1 (car info)))
	  (cond
	   ((eq a t)
	    (save-excursion
	      (goto-char b)
	      (setq val (current-indentation)))
	    (setq base-value val))
	   ((symbolp b)
	    (setq val (matlab-var-value b))
	    (cond
	     ((eq a '=)
	      (cond
	       ((null val)
		;; no indentation
		;; set info to nil so  we stop immediately
		(setq base-value nil  ofs nil  info nil))
	       ((eq val t) (setq ofs 0)) ;; indent as normal line
	       (t
		;; The following assume the (t POS) come first!
		(setq ofs val  base-value 0)
		(setq info nil))))	;; ? stop now
	     ((eq a '+) (setq ofs (+ ofs val)))
	     ((eq a '-) (setq ofs (- ofs val)))
	     (t
	      (error "matlab-calculate-indent invalid a a=%s b=%s" a b))))
	   (t
	    (error "matlab-calculate-indent invalid elt: a=%s b=%s" a b))))
	 (t
	  (error "matlab-calculate-indent invalid elt %s" elt)))
	(matlab-debug "a=%s b=%s val=%s base-value=%s ofs=%s"
		  a b val base-value ofs)
	(setq info (cdr info)))
      ;; return value:
      (matlab-debug "at end:  base-value: %s    ofs: %s" base-value ofs)

      (cond
       ((or (null base-value)(null ofs))
	nil)
       ((and (numberp base-value)(numberp ofs))
	(matlab-debug "base (%d) + ofs (%d) = %d"
		  base-value ofs (+ base-value ofs))
	(+ base-value ofs)) ;; return value
       (t
	(error "matlab-calculate-indent:  Help.  base-value=%s ofs=%s"
	       base-value ofs)
	nil)))))


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

;; If at the beginning of the buffer: set indent to 0
;; If the line contains a block beginning word, (+ matlab-current-indent ...)
;; If the line contains 'end', de-indent
;; otherwise indent to current-indent
;; (defun matlab-indent-line ()
;;   "Update the indentation"
;;   (save-excursion
;;     (let ((begin-re (matlab-rx begin-block))
;; 	  (end-re "end")
;; 	  (eol (save-excursion (end-of-line) (point))))
;;       (cond
;;        ((bobp) (setq matlab-current-indent 0))
;;        ((re-search-forward begin-re eol 'keep-point)
;; 	(message "Begin block")
;; 	(setq matlab-current-indent
;; 	      (+ matlab-current-indent matlab-default-tab-width)))
;;        ((re-search-forward end-re eol 'keep-point)
;; 	(message "End Block")
;; 	(setq matlab-current-indent
;; 	      (- matlab-current-indent matlab-default-tab-width)))
;;        (t (message "None of the above")))))
;;   (let* ((eol (save-excursion (end-of-line) (point)))
;; 	 (ws (save-excursion
;; 	       (beginning-of-line)
;; 	       (re-search-forward "[^[:space:]]" eol 'keep-point)
;; 	       (point))))
;;     (beginning-of-line)
;;     (delete-char (- (- ws (point)) 1)))
;;   (indent-to matlab-current-indent)
;;   (message (format "Current Indent: %d" matlab-current-indent)))

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
  (set (make-local-variable 'indent-line-function) #'matlab-indent-line)
  
  (setq major-mode 'matlab-mode)
  (run-hooks 'matlab-mode-hook)
)

;; Set the file extension that triggers this mode.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

(provide 'matlab-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    line-wrap.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    This file contains a function which truncates lines that
;;		    are too long.
;;
;; CREATED:	    10/24/2017
;;
;; LAST EDITED:	    12/27/2017
;;;

(defgroup line-wrap nil
  "Group for the variables that pertain to the functions in line-wrap.el")

(defcustom line-wrap-column-number 79
  "The number that line-wrap truncates lines at."
  :type 'integer
  :group 'line-wrap)

(defcustom line-wrap-delimit-next-line nil
  "Determines whether or not to delimit the line before wrapping it."
  :type 'boolean
  :group 'line-wrap)

(defcustom line-wrap-truncate-unbroken-strings t
  "When non-nil, line-wrap() will truncate any lines that consist of long,
unbroken character strings, for example, URLs."
  :type 'boolean
  :group 'line-wrap)

(defcustom line-wrap-delimiter 'line-wrap-default-delimiter-hook
  "A function that determines which delimiter to use, if
`line-wrap-delimit-next-line' is non-nil. This function should return a cell of
the form (string . length) where <string> is a string to be inserted before
truncating the line, and <length> is the length of the delimiter. Ideally, the
delimiter string should be as short as possible. The behaviour will be
undefined if the delimiter is longer than `line-wrap-column-number' or if
<length> is longer than the actual length of the string."
  :options '(line-wrap-default-delimiter-hook)
  :group 'line-wrap)

(defcustom line-wrap-indentation 'line-wrap-default-indentation-hook
  "Determines how to indent truncated lines in the current mode.
This function takes no arguments, and should indent the next line accordingly."
  :options '(line-wrap-default-indentation-hook)
  :group 'line-wrap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    line-wrap
;;
;; DESCRIPTION:	    This function truncates the lines to a length of less than
;;		    or equal to line-wrap-column-number. Non-whitespace
;;		    characters are considered significant, and line-wrap will
;;		    not truncate a line in the middle of a word.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun line-wrap ()
  "Wrap the lines in the region, so that no line extends past the column with
number `line-wrap-column-number'."
  (interactive)
  (setq deactivate-mark t)
  (if (not (use-region-p))
      (error "There is no valid region to operate on."))
  ;; Begin here.
  (save-excursion
    (let ((reg-beg (region-beginning))
	  (reg-end (region-end)))
      (goto-char reg-beg)
      (let* ((bol (save-excursion (beginning-of-line) (point)))
      	     (eol (save-excursion (end-of-line) (point)))
      	     cell string length)
	;; If we need to delimit the line, initialize the delimiter.
      	(if line-wrap-delimit-next-line
      	    (progn
      	      (setq cell (funcall line-wrap-delimiter))
      	      (setq string (car cell))
      	      (setq length (cdr cell))
      	      (if (cdr '(length))
		  (error
		   "%s%s%s%s"
		   "The list returned from line-wrap-delimiter "
		   "must return a cons cell pair of the form (string . "
		   "length). Please see the documentation for line-wrap-"
		   "delimiter for more informtation."))
	      t))
	(goto-char reg-end)
	(if (= (point) (point-max))
	    (setq reg-end (point-max))
	  (progn
	    (setq reg-end (save-excursion (end-of-line) (point)))))
	(goto-char reg-beg)
      	(while (< (point) reg-end) ;; Begin
      	  (line-wrap-helper bol eol string length)
	  (setq bol (save-excursion (beginning-of-line) (point)))
	  (setq eol (save-excursion (end-of-line) (point)))))))
  (deactivate-mark t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    line-wrap-helper
;;
;; DESCRIPTION:	    This function is a helper for line-wrap().
;;
;; ARGUMENTS:	    bol: The beginning of the current line.
;;		    eol: The end of the current line.
;;		    string: The string with which to delimit, if necessary.
;;		    length: The length of the string.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun line-wrap-helper (bol eol string length)
  "This function is simply a helper for line-wrap, to make things a little
prettier."
  ;; Only manipulate the lines which extend past the column.
  (if (> eol (+ bol line-wrap-column-number))
      (progn
	(goto-char (+ bol line-wrap-column-number))
	(skip-chars-backward "^[:space:]")
	(if (not (= (point) bol))
	    (progn
	      ;; If we need to delimit, do so now.
	      ;; TODO: Line delimiting is broken
	      (if line-wrap-delimit-next-line
		  (save-excursion
		    (skip-chars-backward " \t")
		    (while (> (+ (point) length)
			      line-wrap-column-number)
		      (skip-chars-backward "^[:space:]")
		      (skip-chars-backward " \t"))
		    (insert " " string)))
	      ;; Then truncate the line.
	      (insert "\n")
	      (if line-wrap-indentation
		  (funcall line-wrap-indentation)
		(indent-for-tab-command))
	      (previous-line)
	      (beginning-of-line)
	      (delete-trailing-whitespace bol eol))
	  ;; We may still want to cut the line, though.
	  (if line-wrap-truncate-unbroken-strings
	      (progn
		(goto-char (+ bol line-wrap-column-number))
		(insert "\n")
		(if line-wrap-indentation
		    (funcall line-wrap-indentation)
		  (indent-for-tab-command))))))
    (next-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    line-wrap-test-lines
;;
;; DESCRIPTION:	    This function traverses the region to test if any of the
;;		    lines extend beyond the limit. If so, the function quits,
;;		    deactivates the region, and prints a message.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    Deactivates the region after executing.
;;;
(defun line-wrap-test-lines ()
  "Determines if there are any lines in the region which extend beyond
`line-wrap-column-number', and stops on the first one found."
  (interactive)
  (if (not (use-region-p))
      (error "There is no valid region to operate on."))
  (let ((bor (region-beginning))
	(eor (region-end))
	eol)
    (goto-char bor)
    (catch 'break
      (while (< (setq eol (save-excursion (end-of-line) (point))) eor)
	(if (> eol
	       (+ (setq bol (save-excursion (beginning-of-line) (point)))
		  line-wrap-column-number))
	    (throw 'break t))
	(next-line)))
    (goto-char eol)
    (deactivate-mark t)
    (if (> eol (+ bol line-wrap-column-number))
	(message (format "Current line length is %d; Limit is %d"
			 (- eol bol) line-wrap-column-number)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    line-wrap-default-delimiter-hook
;;
;; DESCRIPTION:	    This function returns a cons cell containing a string and
;;		    string length with which to delimit newlines that are
;;		    inserted into the buffer by line-wrap(). It is the default
;;		    value for line-wrap-delimiter, but this variable may be
;;		    changed by the user.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    (<string> . <length>)
;;
;; NOTES:	    none.
;;;
(defun line-wrap-default-delimiter-hook ()
  "Returns a cons cell of the form (string . length) to determine how to
delimit newlines placed into the buffer."
  '("\\" . 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    line-wrap-default-indentation-hook
;;
;; DESCRIPTION:	    This function indents the line according to the major-mode.
;;		    This is the default value for the hook
;;		    line-wrap-indentation, but it can be changed by the user.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    Indents the line accordingly.
;;;
(defun line-wrap-default-indentation-hook ()
  "Indents the line according to the major mode. The default value for
`line-wrap-indentation'. See documentation for further details."
  (cond
   ((or (eq major-mode 'fundamental-mode)
	(eq major-mode 'text-mode))
    nil)
   (t
    (indent-for-tab-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

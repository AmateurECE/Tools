;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    line-wrap.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    This file contains a function which truncates lines that are
;;		    too long.
;;
;; CREATED:	    10/24/2017
;;
;; LAST EDITED:	    10/24/2017
;;;

(defgroup line-wrap nil
  "Group for the variables that pertain to the functions in line-wrap.el")

(defcustom line-wrap-column-number 80
  "The number that line-wrap truncates lines at."
  :type 'integer
  :group 'line-wrap)

(defcustom line-wrap-delimit-next-line nil
  "Determines whether or not to delimit the line before wrapping it."
  :type 'boolean
  :group 'line-wrap)

(defcustom line-wrap-delimiter 'line-wrap-default-delimiter-hook
  "A function that determines which delimiter to use, if
`line-wrap-delimit-next-line' is non-nil. This function should return a cell of
the form (string . length) where <string> is a string to be inserted before
truncating the line, and <length> is the length of the delimiter. Ideally, the
delimiter string should be as short as possible. The behaviour will be undefined
if the delimiter is longer than `line-wrap-column-number' or if <length> is
longer than the actual length of the string."
  :options '(line-wrap-default-delimiter-hook)
  :group 'line-wrap)

(defvar line-wrap-debug t
  "If non-nil, debugging messages will be printed for line-wrap functions.")

(defun lw-debug (strmsg)
  "Debug messaging for line-wrap functions."
  (if line-wrap-debug
	(message strmsg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Wrap the lines in the region"
  (interactive)
  (setq deactivate-mark t)
  (if (not (use-region-p))
      (error "There is no valid region to operate on."))
  (save-excursion
    (let ((reg-beg (region-beginning))
	  (reg-end (region-end)))
      (lw-debug (format "Region beg: %d; Region end: %d" reg-beg reg-end))
      (goto-char reg-beg)
      (let* ((bol (save-excursion (beginning-of-line) (point)))
      	     (eol (save-excursion (end-of-line) (point)))
      	     cell string length)
      ;; 	(if line-wrap-delimit-next-line
      ;; 	    (progn
      ;; 	      (setq cell (line-wrap-delimiter))
      ;; 	      (setq string (car cell))
      ;; 	      (setq length (cdr cell))
      ;; 	      (if (cdr length)
      ;; 		  (error "%s%s%s%s"
      ;; 			 "The list returned by the hook in line-wrap-delimiter "
      ;; 			 "must return a cons cell pair of the form (string . "
      ;; 			 "length). Please see the documentation for line-wrap-"
      ;; 			 "delimiter for more informtation."))
      ;; 	      t))
      	(while (<= eol reg-end)
      	  (if (> eol (+ bol line-wrap-column-number))
      	      (goto-char (+ bol line-wrap-column-number))
      	    (lw-debug
      	     (format "Going to char %d" (+ bol line-wrap-column-number)))
      	    (skip-chars-backward "[^[:space:]]")
      	    (if line-wrap-delimit-next-line
      		(save-excursion
      		  (skip-chars-backward " \t")
      		  (while (> (+ point length) line-wrap-column-number)
      		    (skip-chars-backward "[^[:space:]]")
      		    (skip-chars-backward " \t"))
      		  (insert " " string)))
      	    ;; (insert "\n")
      	    (indent-for-tab-command)
      	    (previous-line)
      	    (beginning-of-line)
      	    (delete-trailing-whitespace bol eol))
	  (next-line)
	  (setq bol (save-excursion (beginning-of-line) (point)))
	  (setq eol (save-excursion (end-of-line) (point)))))))
  (deactivate-mark t))

(defun line-wrap-default-delimiter-hook ()
  "Returns a cons cell of the form (string . length) to determine how to delimit
newlines placed into the buffer."
    (?\ . 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

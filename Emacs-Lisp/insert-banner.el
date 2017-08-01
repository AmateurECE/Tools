;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    insert-banner.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    File containing defuns for documenting code. These commands
;;		    also have key bindings so that they can easily be used
;;		    from any buffer. They were even used to document this file!
;;
;; CREATED:	    06/16/2017
;;
;; LAST EDITED:	    06/16/2017
;;;

;; ====== NOTE: ======
;; I think that good documentation is very important. If you have any
;; recommendations for my documentation style, please don't hesitate to let me
;; know.
;; ===================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definitions
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    generic-file-banner
;;
;; DESCRIPTION:	    Function for inserting a generic file-banner. This function
;;		    is called whenever the buffer it is called from is in one
;;		    of the major-modes that supports this. These modes are:
;;			* c-mode
;;			* sh-mode
;;			* txt-mode
;;			* perl-mode
;;			* emacs-lisp-mode
;;			* asm-mode'
;;		    ' - Note: These modes are supported by this function, but
;;		    may have specific implementations for function or section
;;		    headers.
;;		    All other supported modes have individual implementations.
;;
;; ARGUMENTS:	    nl: (string) -- char printed at new line.
;;		    sym: (string) -- standard comment char.
;;		    stt: (string) -- char that starts & ends a comment. Only
;;			used in c-mode.
;;
;; RETURN:	    nil.
;;
;; NOTES:	    none.
;;;
(defun generic-file-banner (nl sym stt)
  "File Banner function for many major modes"
  (interactive)

  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 80)
    (setq iter 79)
    )
  
  (let (val)
    (dotimes (num iter val)
      (insert sym)
      ))

  (insert "\n" nl)
  (if (string-equal sym "#")
      (insert " NAME:		    " name "\n")
    (insert " NAME:	    " name "\n")
    )
  (insert nl "\n" nl)
  (insert " AUTHOR:	    Ethan D. Twardy\n")
  (insert nl "\n" nl)
  (insert " DESCRIPTION:	    \n")
  (insert nl "\n" nl)
  (insert " CREATED:	    " date)
  (insert nl "\n" nl)
  (insert " LAST EDITED:	    " date)
  (if (string-equal sym ";")
      (insert sym sym sym)
    (insert nl sym sym)
    )
  (when (not (null stt)) (insert stt))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-file-banner
;;
;; DESCRIPTION:	    This function is responsible for inserting the pretty banner
;;		    that you see at the beginning of every one of my source
;;		    files.
;;
;; ARGUMENTS:	    name: The name of the function
;;
;; RETURN:	    void.
;;
;; NOTES:	    
;;;
(defun insert-file-banner ()
  "Insert a banner at the top of a file"
  (interactive)
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)
  (setq date (shell-command-to-string "date +%m/%d/%Y"))
  (setq name (match-string 0 buffer-file-name))
  (string-match "/\\([^/]*\\)$" buffer-file-name)
  (setq name (match-string 1 buffer-file-name))
  
(unless (cond ((eq major-mode 'c-mode)
	       (progn
		 (setq nl " *")
		 (setq sym "*")
		 (setq stt "/")
		 )
	       )
	      ((or (eq major-mode 'emacs-lisp-mode)
		   (eq major-mode 'asm-mode))
	       (progn
		 (setq nl ";;")
		 (setq sym ";")
		 )
	       )
	      )
  (progn
    (setq nl "#")
    (setq sym "#"))
  )
  (generic-file-banner nl sym stt)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-function-header
;;
;; DESCRIPTION:	    This function inserts the headers that are at the beginning
;;		    of each one of my functions (like here.)
;;
;; ARGUMENTS:	    name: The name of the function.
;;
;; RETURN:	    void.
;;
;; NOTES:	    
;;;
(defun insert-function-header (name)
  "Insert a header at the top of a function"
  (interactive "sFunction-Name: \n")
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)
  
(unless (cond ((eq major-mode 'c-mode)
	       (progn
		 (setq nl " *")
		 (setq sym "*")
		 (setq stt "/")
		 )
	       )
	      ((or (eq major-mode 'emacs-lisp-mode)
		   (eq major-mode 'asm-mode))
	       (progn
		 (setq nl ";;")
		 (setq sym ";")
		 )
	       )
	      )
  (progn
    (setq nl "#")
    (setq sym "#"))
  )
    
  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 80)
    (setq iter 79)
    )
  (let (val)
    (dotimes (num iter val)
      (insert sym)
      )
    )
  (insert "\n" nl)
  (insert " FUNCTION:	    " name "\n" nl "\n" nl)
  (insert " DESCRIPTION:	    " "\n" nl "\n" nl)
  (insert " ARGUMENTS:	    " "\n" nl "\n" nl)
  (insert " RETURN:	    " "\n" nl "\n" nl)
  (insert " NOTES:	    \n")
  (if (string-equal sym ";")
      (insert sym sym sym)
    (insert nl sym sym)
    )
  (when (not (null stt)) (insert stt))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-section-header
;;
;; DESCRIPTION:	    This function inserts the headers that separate each part
;;		    of the source code. Section guidelines are outlined in
;;		    each of the respective languages' coding style guidelines.
;;
;; ARGUMENTS:	    name: The name of the section.
;;
;; RETURN:	    void.
;;
;; NOTES:	    
;;;
(defun insert-section-header (name)
  "Insert a section header"
  (interactive "sSection-Name: \n")
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)

(unless (cond ((eq major-mode 'c-mode)
	       (progn
		 (setq nl " *")
		 (setq sym "*")
		 (setq stt "/")
		 )
	       )
	      ((or (eq major-mode 'emacs-lisp-mode)
		   (eq major-mode 'asm-mode))
	       (progn
		 (setq nl ";;")
		 (setq sym ";")
		 )
	       )
	      )
  (progn
    (setq nl "#")
    (setq sym "#"))
  )
      
  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 80)
    (setq iter 79)
    )

  (let (val)
    (dotimes (num iter val)
      (insert sym)
      )
    )

  (insert "\n" nl " " name "\n")
  (if (string-equal sym ";")
      (insert sym sym sym)
    (insert nl sym sym)
    )
  (when (not (null stt)) (insert stt))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

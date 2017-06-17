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
;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definitions
;;;;;;

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
;;;;;;
(defun insert-file-banner (name)
  "Insert a banner at the top of a file"
  (interactive "sFilename: \n")
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)
  (setq date (shell-command-to-string "date +%m/%d/%Y"))

  (when (eq major-mode 'c-mode)
    (setq nl " *")
    (setq sym "*")
    (setq stt "/")
    )
  (when (or (eq major-mode 'sh-mode)
	    (eq major-mode 'perl-mode)
	    (eq major-mode 'makefile-bsdmake-mode))
    (setq nl "#")
    (setq sym "#")
    )
  (when (or (eq major-mode 'emacs-lisp-mode)
	    (eq major-mode 'asm-mode))
    (setq nl ";;")
    (setq sym ";")
    )
  
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
  (if (or (eq major-mode 'perl-mode)
	  (eq major-mode 'sh-mode)
	  (eq major-mode 'makefile-bsdmake-mode))
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
  (if (or (eq major-mode 'emacs-lisp-mode)
	  (eq major-mode 'asm-mode))
      (insert sym sym sym)
    (insert nl sym sym)
    )
  (when (not (null stt)) (insert stt))
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
;;;;;;
(defun insert-function-header (name)
  "Insert a header at the top of a function"
  (interactive "sFunction-Name: \n")
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)
  
  (when (eq major-mode 'c-mode)
    (setq nl "\n *")
    (setq sym "*")
    (setq stt "/")
    )
  (when (or (eq major-mode 'sh-mode)
	    (eq major-mode 'perl-mode)
	    (eq major-mode 'makefile-bsdmake-mode))
    (setq nl "\n\#")
    (setq sym "\#")
    )
  (when (or (eq major-mode 'emacs-lisp-mode)
	    (eq major-mode 'asm-mode))
    (setq nl "\n;;")
    (setq sym ";")
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
  (insert nl)
  (insert " FUNCTION:	    " name nl nl)
  (insert " DESCRIPTION:	    " nl nl)
  (insert " ARGUMENTS:	    " nl nl)
  (insert " RETURN:	    " nl nl)
  (insert " NOTES:	    ")
  (if (or (eq major-mode 'emacs-lisp-mode)
	  (eq major-mode 'asm-mode))
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
;;;;;;
(defun insert-section-header (name)
  "Insert a section header"
  (interactive "sSection-Name: \n")
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)

  (when (eq major-mode 'c-mode)
    (setq nl " *")
    (setq sym "*")
    (setq stt "/")
    )
  (when (or (eq major-mode 'sh-mode)
	    (eq major-mode 'perl-mode)
	    (eq major-mode 'makefile-bsdmake-mode))
    (setq nl "\#")
    (setq sym "\#")
    )
  (when (or (eq major-mode 'emacs-lisp-mode)
	    (eq major-mode 'asm-mode))
    (setq nl ";;")
    (setq sym ";")
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
  (if (or (eq major-mode 'emacs-lisp-mode)
	  (eq major-mode 'asm-mode))
      (insert sym sym sym)
    (insert nl sym sym)
    )
  (when (not (null stt)) (insert stt))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

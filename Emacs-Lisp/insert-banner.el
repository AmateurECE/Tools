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
;; Variable Definitions
;;;

(defcustom file-banner-license-notice nil
  "If this is set to t, insert-file-banner will insert a license notice."
  :type 'boolean
  )

(defconst file-copyright-notice
  "Copyright Date, Ethan D. Twardy"
  )

(defconst file-license-notice
  "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definitions
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Banners
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
;;			* latex-mode'
;;			* matlab-mode
;;			* python-mode'
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
  (when (not (eq file-banner-license-notice nil))
    (progn
      (insert nl "\n" nl " ")
      (let ((str file-copyright-notice)
	    (date (shell-command-to-string "date +%Y")))
	(setq str (replace-regexp-in-string "Date" date str))
	(insert (replace-regexp-in-string "\n" "" str) "\n")
	(insert nl "\n")
	)
      (dolist (line (split-string file-license-notice "\n"))
	(insert nl " " line "\n")
	)
      )
    )
  (if (string-equal sym ";")
      (insert sym sym sym)
    (insert nl sym sym)
    )
  (when (not (null stt)) (insert stt))
  (goto-char 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    ubt-file-banner
;;
;; DESCRIPTION:	    Insert a file banner for the U-Boot scripting language.
;;
;; ARGUMENTS:	    nl: (string) -- char printed at new line.
;;		    sym: (string) -- standard comment char.
;;		    stt: (string) -- char that starts & ends a comment. Only
;;			used in c-mode.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun ubt-file-banner (nl sym stt)
  "Insert a file banner at the top of a U-Boot Script file"

  (setq iter 80)
  (let (val)
    (dotimes (num iter val)
      (insert sym)
      ))

  (insert "\n" nl)
  (insert " NAME:		    " name "\n")
  (insert nl "\n" nl)
  (insert " AUTHOR:	    Ethan D. Twardy\n")
  (insert nl "\n" nl)
  (insert " DESCRIPTION:	    \n")
  (insert nl "\n" nl)
  (insert " CREATED:	    " date)
  (insert nl "\n" nl)
  (insert " LAST EDITED:	    " date)
  (insert nl "\n" nl)
  (insert " DEPENDENCIES:	    \n")
  (insert nl sym sym)
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
  
  (cond ((or (eq major-mode 'c-mode) (eq major-mode 'asm-mode))
	 (generic-file-banner " *" "*" "/"))
	((eq major-mode 'emacs-lisp-mode)
	 (generic-file-banner ";;" ";" nil))
	((or (eq major-mode 'latex-mode) (eq major-mode 'matlab-mode))
	 (generic-file-banner "%" "%" nil))
	((eq major-mode 'ubt-mode)
	 (ubt-file-banner "#" "#" nil))
	(t (generic-file-banner "#" "#" nil)) ;; Default case
	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Header
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    generic-function-header
;;
;; DESCRIPTION:	    Function for inserting a generic function-banner. This
;;		    function is called whenever the buffer it is called from is
;;		    one of the major-modes that supports this. These modes are:
;;			* c-mode
;;			* sh-mode
;;			* txt-mode
;;			* perl-mode
;;			* emacs-lisp-mode
;;			* matlab-mode
;;		    ' - Note: These modes are supported by this function, but
;;		    may have specific implementations for file or section
;;		    headers.
;;		    All other supported modes have individual implementations.
;;
;; ARGUMENTS:	    
;;
;; RETURN:	    
;;
;; NOTES:	    
;;;
(defun generic-function-header (nl sym stt)
  "Inserts the generic function header"
  
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
;; FUNCTION:	    latex-function-header
;;
;; DESCRIPTION:	    Inserts a function header for LaTeX.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun latex-function-header ()
  "Inserts a command header for LaTeX."
  
  (let (val)
    (dotimes (num 80 val)
      (insert "%")))
  (insert "% Command:	    " name "\n")
  (insert "% Function:	    \n")
  (insert "% Arguments:	    \n")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    asm-function-header
;;
;; DESCRIPTION:	    Insert an assembly subroutine header.
;;
;; ARGUMENTS:	    
;;
;; RETURN:	    
;;
;; NOTES:	    
;;;
(defun asm-function-header ()
  "Insert an Assembly subroutine header"

  (insert "/")
  (let (val)
    (dotimes (num 80 val)
      (insert "*")))
  (insert "\n *")
  (insert " SUBROUTINE:	    " name "\n *\n *")
  (insert " DESCRIPTION:	    \n *\n *")
  (insert " REGISTER USAGE:  " "\n *\n *")
  (insert " RETURN:	    \n ***/")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    python-function-header
;;
;; DESCRIPTION:	    Inserts a PyDoc Function "header" inside of a function
;;		    definition.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun python-function-header ()
  "Inserts a PyDoc function header in the Google style."
  ;; Not interactive.
  (insert "\"\"\"" name ":\n")
  (indent-for-tab-command)
  (setq currpos (point))
  (insert "\n\n")
  (indent-for-tab-command)
  (insert "Args:\n")
  (indent-for-tab-command)
  (insert "\t\n\n")
  (indent-for-tab-command)
  (insert "Returns:\n")
  (indent-for-tab-command)
  (insert "\t\n\n")
  (indent-for-tab-command)
  (insert "Raises:\n")
  (indent-for-tab-command)
  (insert "\t\n")
  (indent-for-tab-command)
  (insert "\"\"\"")
  (goto-char currpos)
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
  
(cond ((eq major-mode 'c-mode)
       (generic-function-header " *" "*" "/"))
      ((eq major-mode 'emacs-lisp-mode)
       (generic-function-header ";;" ";" nil))
      ((eq major-mode 'matlab-mode)
       (generic-function-header "%" "%" nil))
      ((eq major-mode 'latex-mode)
       (latex-function-header))
      ((eq major-mode 'asm-mode)
       (asm-function-header))
      ((eq major-mode 'python-mode)
       (python-function-header))
      (t (generic-function-header "#" "#" nil)) ;; Default case.
      )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section Header
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    generic-section-header
;;
;; DESCRIPTION:	    Inserts a generic section-header. This function exists
;;		    mostly in case I want to change the format for a particular
;;		    mode in the future. Currently it supports all major modes.
;;
;; ARGUMENTS:	    nl: (string) -- char printed at new line.
;;		    sym: (string) -- standard comment char.
;;		    stt: (string) -- char that starts & ends a comment. Only
;;			used in c-mode.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun generic-section-header (nl sym stt)
  "Insert a generic-section header."
  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 80)
    (setq iter 79))

  (let (val)
    (dotimes (num iter val)
      (insert sym)))

  (insert "\n" nl " " name "\n")
  (if (string-equal sym ";")
      (insert sym sym sym)
    (insert nl sym sym))
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

  (cond ((or (eq major-mode 'c-mode) (eq major-mode 'asm-mode))
	 (generic-section-header " *" "*" "/"))
	((eq major-mode 'emacs-lisp-mode)
	 (generic-section-header ";;" ";" nil))
	((or (eq major-mode 'latex-mode) (eq major-mode 'matlab-mode))
	 (generic-section-header "%" "%" nil))
	(t (generic-section-header "#" "#" nil))) ;; Default case.
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class Docs
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    python-class-header
;;
;; DESCRIPTION:	    Insert a PyDoc class header in the Google style.
;;
;; ARGUMENTS:	    name: The name of the class.
;;
;; RETURN:	    None.
;;
;; NOTES:	    None.
;;;
(defun python-class-header (name)
  "Inset a Google Style Python Class Header."
  (let ((name name))
    (insert "\"\"\"" name "\n")
    (indent-for-tab-command)
    (setq currpos (point))
    (insert "\n\n")
    (indent-for-tab-command)
    (insert "Attributes:\n")
    (indent-for-tab-command)
    (insert "\t\n")
    (indent-for-tab-command)
    (insert "\"\"\"")
    (goto-char currpos)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-class-header
;;
;; DESCRIPTION:	    Insert a header for a class.
;;
;; ARGUMENTS:	    name: The name of the class.
;;
;; RETURN:	    None.
;;
;; NOTES:	    None.
;;;
(defun insert-class-header (name)
  "Insert a class header"
  (interactive "sClass Name: \n")

  (cond ((eq major-mode 'python-mode)
	 (python-class-header name))
	;; Default case
	(t (message "The function for this mode has not yet been implemented!"))
	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

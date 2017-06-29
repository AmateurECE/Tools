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

(defvar matlab-mode-hook nil)

(defvar matlab-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-j") 'newline-and-indent)
    map)
  "Key map for MATLAB Major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

(defconst matlab-font-lock-keywords-1
  (list 
   '("\\<\\(break\\|ca\\(?:se\\|tch\\)\\|dbcont\\|e\\(?:lse\\(?:if\\)?\\|nd\\)\\|for\\|global\\|if\\|otherwise\\|persistent\\|return\\|switch\\|try\\|while\\)\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-face))
  "Minimal highlighting expressions for MATLAB mode")

(defvar matlab-font-lock-keywords matlab-font-lock-keywords-1
  "Default Highlighting for MATLAB Keywords")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;

(defun matlab-mode ()
  "Major-mode for editing MATLAB Code."
  (interactive)
  (kill-all-local-variables)
;  (set-syntax-table matlab-mode-syntax-table)
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

(provide 'matlab-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
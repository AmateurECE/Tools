;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\
;; NAME:	    ubt-mode.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    Emacs derived mode for editing .ubt files. Basically the
;;		    same as sh-mode, but it triggers on a different filename
;;		    and has a little extra syntactic sugar.
;;
;; CREATED:	    08/16/2017
;;
;; LAST EDITED:	    08/16/2017
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLE DEFINITIONS
;;;

;; Add to this variable when new directives are defined.
(defvar ubt-preprocessor-directives
  '("include"))

(defvar ubt-font-lock-preprocessor-directives
  `(
    ("^[[:blank:]]*\\(@\\<include\\>\\)[[:blank:]]*\\(\\sw+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face nil t))
    (,(concat "\\<\\(printenv\\|sete\\(?:nv\\|xpr\\(?:\\.[blsw]\\)?\\)\\)\\>"
	      "[[:blank:]]*\\(\\sw+\\)")
     (2 font-lock-variable-name-face))
    ("\\(itest\\(?:\\.[blsw]\\)?\\)" . font-lock-preprocessor-face)
    ("\\<run\\>" . font-lock-preprocessor-face)
    )
  )

(define-derived-mode ubt-mode sh-mode "U-Boot Script"
  "This mode is for editing U-Boot script files."
  
  (font-lock-add-keywords nil ubt-font-lock-preprocessor-directives)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURAL STATEMENTS
;;;

;; I'm not sure exactly how this works, but it makes Emacs open ubt-mode.el
;; when a buffer that matches '\.ubt' is created.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ubt\\|\\.src\\'" . ubt-mode))

(provide 'ubt-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

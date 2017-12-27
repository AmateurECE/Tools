;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; LAST EDITED:	    12/27/2017
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLE DEFINITIONS
;;;

;; Add to this variable when new directives are defined.
(defvar ubt-preprocessor-directives
  '("include" "define" "if" "ifdef" "ifndef"))

(defvar ubt-font-lock-preprocessor-directives
  `(;; @include -> function face
    ("^[[:blank:]]*\\(@\\<include\\>\\)[[:blank:]]*\\([[:graph:]]+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face nil t))
    ;; vars in env commands -> var name face
    (,(concat
       "\\<"
       (regexp-opt '("env ask" "env ask.bkup" "env delete" "env delete -f"
		     "env edit" "env print" "env print.bkup" "env print -a"
		     "env print.bkup -a" "env set" "env set -f" "env set.bkup"
		     "env set.bkup -f" "setenv" "setexpr" "setexpr.b"
		     "setexpr.l" "setexpr.w" "askenv")
		   t)
       "\\>"
       "[[:blank:]]*\\([[:graph:]]+\\)")
     (2 font-lock-variable-name-face))
    ;; @include directives -> function face
    ("^[[:blank:]]*\\(@\\<define\\>\\)[[:blank:]]*\\([[:alnum:]_]+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face nil t))
    ;; @if & family diretives -> function face
    (,(regexp-opt '("@if" "@ifdef" "@ifndef" "@else" "@endif") t)
     . font-lock-function-name-face)
    ;; itest -> preprocessor face (same as sh-mode test)
    ("\\(itest\\(?:\\.[blsw]\\)?\\)" . font-lock-preprocessor-face)
    ;; run -> builtin face
    ("\\<run\\>" . font-lock-preprocessor-face)))

(define-derived-mode ubt-mode sh-mode "U-Boot Script"
  "This mode is for editing U-Boot script files."
  (font-lock-add-keywords nil ubt-font-lock-preprocessor-directives))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURAL STATEMENTS
;;;

;; Add file extension to auto-mode-alist so that we're recognized.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ubt\\|\\.src\\'" . ubt-mode))

(provide 'ubt-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

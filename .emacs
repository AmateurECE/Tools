;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    .emacs
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    Emacs initialization file.
;;
;; CREATED:	    09/15/2017
;;
;; LAST EDITED:	    02/28/2019
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD EXTERNAL FILES
;;;

;; TODO: Replace with "add-to-list 'load-path"
;; TODO: bytecode compile all extensions
;; TODO: Conditionally load all major modes
(let ((lisp-dir (cond
		 ((file-exists-p "/home/edtwardy/Git/Emacs-Extensions")
		  "/home/edtwardy/Git/Emacs-Extensions/")
		 ((file-exists-p "/Users/ethantwardy/Git/Emacs-Extensions")
		  "/Users/ethantwardy/Git/Tools/Emacs-Extensions/")
		 (t
		  (error "User's Emacs-Lisp directory could not be found.")))))
  ;; The point is to uncomment these if I find I'll be using them for extended
  ;; periods of time. This keeps emacs free to do other things on startup.
  (load-file (concat lisp-dir "insert-banner.el"))
  (load-file (concat lisp-dir "line-wrap.el"))

  ;; Load custom python.el, if we are loading a python file.
  (let ((loading-python nil))
    (dolist (arg command-line-args)
      (when (string-match "\\.py$" arg)
	(setq loading-python t)))
    (when (eq loading-python t)
      (if (file-exists-p (concat lisp-dir "python.elc"))
	  (load-file (concat lisp-dir "python.elc"))
	(byte-compile-file (concat lisp-dir "python.el") t))))

  ;; (load-file (concat lisp-dir "restart-emacs.el"))
  ;; (load-file (concat lisp-dir "ubt-mode.el"))
  ;; (load-file (concat lisp-dir "dts-mode.el"))
  ;; (load-file (concat lisp-dir "matlab.el"))
  ;; (load-file (concat lisp-dir "yacc-mode.el"))
  ;; (load-file (concat lisp-dir "spice-mode.el"))
  ;; (load-file (concat lisp-dir "markdown-mode.el"))
  (load-file (concat lisp-dir "nxml-hide.el"))

  ;; Dockerfile mode initialization.
  (load-file (concat lisp-dir "s.el/s.el")) ;; needed by dockerfile-mode
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (load-file (concat lisp-dir "dockerfile-mode.el"))

  ;; Load rust-mode
  (add-to-list 'load-path (concat lisp-dir "rust-mode"))
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISCELLANEOUS INITIALIZATION
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(electric-pair-mode t)
 '(inhibit-default-init t)
 '(standard-indent 8)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "color-22"))))
 '(diff-header ((t (:background "color-89"))))
 '(diff-removed ((t (:inherit diff-changed :background "color-88")))))

;; Configure for MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Turn on error-catching.
(setq debug-on-error t)

;; Prevent verilog mode from automatically inserting a newline after every
;; semicolon.
(setq verilog-auto-newline nil)

;; alist additions
(add-to-list 'auto-mode-alist '("\\.gradle" . java-mode))
(add-to-list 'auto-mode-alist '("\\.bash_aliases" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.plist" . xml-mode))
(delete "[Mm]akefile\\'" auto-mode-alist)
(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.html" . nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS
;;;

;; Set undo to \C-z
(global-unset-key (kbd "C-/"))
;; (global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

;; Rebind universal-argument to C-n
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n") 'universal-argument)

(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u u") '(lambda() (interactive) (insert "{\\\"u}")))
(global-set-key (kbd "C-u a") '(lambda() (interactive) (insert "{\\\"a}")))
(global-set-key (kbd "C-u o") '(lambda() (interactive) (insert "{\\\"o}")))
(global-set-key (kbd "C-u U") '(lambda() (interactive) (insert "{\\\"U}")))
(global-set-key (kbd "C-u A") '(lambda() (interactive) (insert "{\\\"A}")))
(global-set-key (kbd "C-u O") '(lambda() (interactive) (insert "{\\\"O}")))
(global-set-key (kbd "C-u s") '(lambda() (interactive) (insert "{\\ss}")))

;; forward-whitespace and backward-whitespace (see below) are shadowed by C-j
;; key binding in LaTeX mode and Asm mode. These hooks fix that.
(add-hook 'latex-mode-hook
	  (lambda()
	    (local-unset-key (kbd "C-j"))))
(add-hook 'asm-mode-hook
	  (lambda()
	    (local-unset-key (kbd "C-j"))))

;; Bindings for forward-whitespace and backward whitespace, etc.
(global-unset-key (kbd "C-k"))
(global-set-key (kbd "C-k") 'forward-whitespace)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") ;; Lamba function that behaves like a mirror of
		'(lambda()  ;; forward-whitespace.
		   (interactive)
		   (cond
		    ((eq (char-before (point)) ?\n)
		     (skip-chars-backward " \n\t"))
		    ((or (eq (char-before (point)) ?\t)
			 (eq (char-before (point)) ?\s))
		     (skip-chars-backward " \t"))
		    (t (progn
			 (while (looking-back "[^[:space:]]" (- (point) 1))
			   (re-search-backward "[^[:space:]]"))
			 (skip-chars-backward " \t"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

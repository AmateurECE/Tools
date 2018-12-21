;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    .emacs
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    Emacs initialization file.
;;
;; CREATED:	    09/15/2017
;;
;; LAST EDITED:	    12/21/2018
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD EXTERNAL FILES
;;;

;; TODO: Replace with "add-to-list 'load-path"
(let ((lisp-dir (cond
		 ((file-exists-p "~/Git/Emacs-Extensions")
		  "~/Git/Emacs-Extensions/")
		 ((file-exists-p "~/Documents/Tools/Emacs-Lisp")
		  "~/Documents/Tools/Emacs-Lisp/")
		 (t
		  (error "User's Emacs-Lisp directory could not be found.")))))
  ;; The point is to uncomment these if I find I'll be using them for extended
  ;; periods of time. This keeps emacs free to do other things on startup.
  (load-file (concat lisp-dir "insert-banner.el"))
  (load-file (concat lisp-dir "line-wrap.el"))
  ;; (load-file (concat lisp-dir "restart-emacs.el"))
  ;; (load-file (concat lisp-dir "ubt-mode.el"))
  ;; (load-file (concat lisp-dir "dts-mode.el"))
  ;; (load-file (concat lisp-dir "matlab.el"))
  ;; (load-file (concat lisp-dir "yacc-mode.el"))
  ;; (load-file (concat lisp-dir "spice-mode.el"))
  ;; (load-file (concat lisp-dir "markdown-mode.el"))
  (load-file (concat lisp-dir "nxml-hide.el"))
  ;; (load-file (concat lisp-dir "s.el/s.el")) ;; needed by dockerfile-mode
  ;; (load-file (concat lisp-dir "dockerfile-mode.el"))
  t)

(add-to-list 'load-path "~/Git/Emacs-Extensions/rust-mode")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

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

;; GNUPlot-mode initialization
(autoload 'gnuplot-mode "gnuplot"
  "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot"
  "open a buffer in gnuplot mode" t)
(setq auto-mode-alist
      (append '(("\\.gp$" . gnuplot-mode))
	      auto-mode-alist))
(global-set-key [(f9)] 'gnuplot-make-buffer)

;; Turn on error-catching.
(setq debug-on-error t)

;; Prevent verilog mode from automatically inserting a newline after every
;; semicolon.
(setq verilog-auto-newline nil)

;; alist additions
(add-to-list 'auto-mode-alist '("\\.bash_aliases" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.plist" . xml-mode))
(delete "[Mm]akefile\\'" auto-mode-alist)
(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.html" . nxml-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS
;;;

;; Set undo to \C-z
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

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

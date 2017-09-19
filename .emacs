;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    .emacs
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    Emacs initialization file.
;;
;; CREATED:	    09/15/2017
;;
;; LAST EDITED:	    09/15/2017
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
 ;;'(font-lock-comment-face ((t (:foreground "red"))))
 )

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

;;; Load files
(if (file-exists-p "~/misc/Tools/Emacs-Lisp/insert-banner.el")
    (load-file "~/misc/Tools/Emacs-Lisp/insert-banner.el")
  (load-file "~/Git/Tools/Emacs-Lisp/insert-banner.el"))
(if (file-exists-p "~/misc/Tools/Emacs-Lisp/ubt-mode.el")
    (load-file "~/misc/Tools/Emacs-Lisp/ubt-mode.el")
  (load-file "~/Git/Tools/Emacs-Lisp/ubt-mode.el"))
(if (file-exists-p "~/misc/Tools/Emacs-Lisp/dts-mode.el")
    (load-file "~/misc/Tools/Emacs-Lisp/dts-mode.el")
  (load-file "~/Git/Tools/Emacs-Lisp/dts-mode.el"))
(if (file-exists-p "~/misc/Tools/Emacs-Lisp/restart-emacs.el")
    (load-file "~/misc/Tools/Emacs-Lisp/restart-emacs.el")
  (load-file "~/Git/Tools/Emacs-Lisp/restart-emacs.el"))
(if (file-exists-p "~/misc/Tools/Emacs-Lisp/matlab-mode.el")
    (load-file "~/misc/Tools/Emacs-Lisp/matlab-mode.el")
  (load-file "~/Git/Tools/Emacs-Lisp/matlab-mode.el"))

;;; Key bindings
;; Set undo to \C-z
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

;; Bindings for forward-whitespace and backward whitespace, etc.
(global-unset-key (kbd "C-k"))
(global-set-key (kbd "C-k") 'forward-whitespace)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") ;; Lamba function that behaves like a mirror of
		'(lambda()  ;; forward-whitespace.
		   (interactive)
		   (cond ((eq (char-before (point)) 10) ;; Newlines
			  (skip-chars-backward " \n\t"))
			 ((or (eq (char-before (point)) 9) ;; Tabs
			      (eq (char-before (point)) 32)) ;; Spaces
			  (skip-chars-backward " \t"))
			 (t
			  (progn
			    (while (looking-back "[^[:space:]]" (- (point) 1))
			      (re-search-backward "[^[:space:]]")
			      )
			    (skip-chars-backward " \t")))
			 )
		   )
		)

;; Set key bindings for inserting headers and banners
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-b b"))
(global-set-key (kbd "C-b b") 'insert-file-banner)
(global-unset-key (kbd "C-b f"))
(global-set-key (kbd "C-b f") 'insert-function-header)
(global-unset-key (kbd "C-b h"))
(global-set-key (kbd "C-b h") 'insert-section-header)
(global-unset-key (kbd "C-b c"))
(global-set-key (kbd "C-b c") 'insert-class-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Load files
(load-file "~/misc/Tools/Emacs-Lisp/insert-banner.el")
(load-file "~/cc_tools/uprep/ubt-mode.el")
;;(load-file "~/misc/Tools/Emacs-Lisp/matlab-mode.el")
(load-file "/home/etwardy/Downloads/gnuplot.el")

;;; Key bindings
;; Set undo to \C-z
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

;; Set key bindings for inserting headers and banners
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-b b"))
(global-set-key (kbd "C-b b") 'insert-file-banner)
(global-unset-key (kbd "C-b f"))
(global-set-key (kbd "C-b f") 'insert-function-header)
(global-unset-key (kbd "C-b h"))
(global-set-key (kbd "C-b h") 'insert-section-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

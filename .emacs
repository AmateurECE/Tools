(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-default-init t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Load files
(load-file "~/misc/Tools/insert-banner.el")
(load-file "~/misc/Tools/matlab-mode.el")

;;; Key bindings
; Set undo to \C-z
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

; Set key bindings for inserting headers and banners
(global-unset-key (kbd "C-b"))
(global-set-key (kbd "C-b") 'insert-file-banner)
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'insert-function-header)
(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h") 'insert-section-header)

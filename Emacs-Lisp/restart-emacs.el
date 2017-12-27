;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAME:	    restart-emacs.el
;;
;; AUTHOR:	    Ethan D. Twardy
;;
;; DESCRIPTION:	    A function to kill and restart emacs--useful for making
;;		    small edits to an emacs script which require reloading the
;;		    file.
;;
;; CREATED:	    09/18/2017
;;
;; LAST EDITED:	    12/27/2017
;;;

(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs (concat "fg ; emacs -nw " name)))

(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (setq name buffer-file-name)
  (let
      ((kill-emacs-hook
	(append kill-emacs-hook
		(list
		 (if (display-graphic-p)
		     #'launch-separate-emacs-under-x
		   #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples from Writing GNU Emacs Extensions
;;
;; This file is called by init.el, so only
;; stuff I really use is not commented out.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 1: Customizing Emacs
;;
;; We won't bother with changing help-command
;; binding, since in Mac DEL key works fine.
;;
;; (global-set-key (kbd "\M-?") 'help-command) 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2: Simple New Commands
;;
(global-set-key (kbd "\C-x\C-n") 'other-window)

(defun other-window-backward (&optional n)
       "Select Nth previous window."
       (interactive "P")
       (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "\C-x\C-p") 'other-window-backward)

(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead one line."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll behind one line."
  (interactive)
  (scroll-behind (prefix-numeric-value n)))

(global-set-key (kbd "M-p") 'scroll-n-lines-behind)
(global-set-key (kbd "M-n") 'scroll-n-lines-ahead)

;; This isn't from the book, but my own function
;; to globally adjust font-size.
(defun change-global-font-size (SIZE)
  "Set global fontsize to SIZE."
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height (* SIZE 10)))

;; (global-set-key (kbd "\C-x q") 'quoted-insert)
;; (defun point-to-top ()
;;   "Put point on top line of window."
;;   (interactive)
;;   (move-to-window-line 0))

;; (defun point-to-bottom ()
;;   "Put point at beginning of last visible line."
;;   (interactive)
;;   (move-to-window-line -1))

;; (defun line-to-top ()
;;   "Move current line to top of window."
;;   (interactive)
;;   (recenter 0))

;; all these key binding can't be changed and doesn't work in current Emacs
;;(global-set-key (kbd "\M-,") 'point-to-top)
;;(global-set-key (kbd "\M-.") 'point-to-bottom)
;;(global-set-key (kbd "\M-!") 'line-to-top)

;; handling symlinks
;; I don't use this
;; (add-hook 'find-file-hooks
;; 	  '(lambda ()
;; 	     (if (file-symlink-p buffer-file-name)
;; 		 (progn
;; 		   (setq buffer-read-only t)
;; 		   (message "File is a symlink")))))

;; (defun visit-target-instead ()
;;   "Replace this buffer with a buffer visiting the link target."
;;   (interactive)
;;   (if buffer-file-name
;;       (let ((target (file-symlink-p buffer-file-name)))
;; 	(if target
;; 	    (find-alternate-file target)
;; 	  (error "Not visiting a symlink")))
;;     (effor "Not visiting a file")))
;; (defun clobber-symlink ()
;;   "Replace symlink with a copy of the file."
;;   (interactive)
;;   (if buffer-file-name
;;       (let ((target (file-symlink-p buffer-file-name)))
;; 	(if target
;; 	    (if (yes-or-no-p (format "Replace %s with %s? "
;; 				     buffer-file-name
;; 				     target))
;; 		(progn

;; 		  (delete-file buffer-file-name)
;; 		  (write-file buffer-file-name)))
;; 	  (error "Not visiting a symlink")))
;;     (error "Not visiting a file")))

;; (global-set-key (kbd "\C-x t") 'visit-target-instead)
;; (global-set-key (kbd "\C-x l") 'clobber-symlink)

;; switch to existing buffers only
;; unfortunately old style of defadvice used, obsolete in current Emacs
;; below comes close...
;; (defun switch-to-buffer--existing-buffer (BUFFER-OR-NAME &optional NORECORD FORCE-SAME-WINDOW)
;;   "When interactive, switch to existing buffers only,
;; unless given a prefix argument."
;;   (interactive
;;    (list (read-buffer "Switch to buffer: "
;; 		      (other-buffer)
;; 		      (null current-prefix-arg))))
;;   (switch-to-buffer other-buffer))
;; (advice-add 'switch-to-buffer :around #'switch-to-buffer--existing-buffer)
;; use below to remove above, since it screws up buffer switching
;;(advice-remove 'switch-to-buffer 'switch-to-buffer--existing-buffer)

;; Chapter 3: Cooperating Commands
;; Unfortunately this code doesn't work due to using old defadvice that is
;; not used in current Emacs.  See elisp manual, Porting old advices

;; (defvar unscroll-point nil
;;   "Cursor position for next call to 'unscroll'.")
;; (defvar unscroll-window-start nil
;;   "Window start for next call to 'unscroll'.")
;; (defadvice scroll-up (before remember-for-unscroll
;; 			     activate compile)
;;   "Remember where we started from, for 'unscroll'."
;;   (if (not (eq last-command 'scroll-up))
;;       (progn
;; 	(setq unscroll-point (point))
;; 	(setq unscroll-window-start (window-start)))))
;; (defun unscroll ()
;;   "Jump to 'unscroll-point' and 'unscroll-window-start'."
;;   (interactive)
;;   (goto-char unscroll-point))
;;   (set-window-start nil unscroll-window-start))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 4 moved to timestamp.el
(provide 'extensions)

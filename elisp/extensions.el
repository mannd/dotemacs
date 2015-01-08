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

(global-set-key (kbd "\C-q") 'scroll-n-lines-behind)
(global-set-key (kbd "\C-z") 'scroll-n-lines-ahead)

(global-set-key (kbd "\C-x\C-q") 'quoted-insert)

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
;; Chapter 4. Searching and Modifying Buffers
(defun insert-current-time ()
  "Insert the current time"
  (interactive "*")
  (insert (format-time-string "%l:%M %p" (current-time))))

;; in recent Emacs, user variables are defined by defcustom, not as below
;; (defvar insert-date-format "%x"
;;   "*Format for \\[insert-date] (c.f. 'format-time-string').")
(defcustom insert-time-format "%X"
  "Format for \\[insert-time] (c.f. 'format-time-string').")
(defcustom insert-date-format "%x"
  "Format for \\[insert-date] (c.f. 'format-time-string').")

(defun insert-time ()
  "Insert the current time according to insert-time-format."
  (interactive "*")
  (insert (format-time-string insert-time-format
			      (current-time))))
(defun insert-date ()
  "Insert the current date according to insert-date-format."
  (interactive "*")
  (insert (format-time-string insert-date-format
			      (current-time))))

;; Writestamps
;; need to use write-file-functions or before-save-hook as local-write-file-hooks
;; is deprecated.
(defun update-writestamps ()
  "Find writestamps and replace them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(let ((regexp (concat "^"
			(regexp-quote writestamp-prefix)
			"\\(.*\\)"
			(regexp-quote writestamp-suffix)
			"$")))
	(while (re-search-forward regexp nil t)
	  (replace-match (format-time-string writestamp-format
					     (current-time))
			 t t nil 1))))))
  nil)

(setq before-save-hook 'update-writestamps)
;(add-hook 'before-save-hook 'update-writestamps)
;(remove-hook 'before-save-hook 'update-writestamps)

(defcustom writestamp-format "%x %X"
  "Format for writestamps (c.f. 'format-time-string').")
(defcustom writestamp-prefix "WRITESTAMP(("
  "Unique string identifying start of writestamp.")
(defcustom writestamp-suffix "))"
   "String that terminates a writestamp.")

(make-local-variable 'first-change-hook)

(defcustom modifystamp-format "%x %X"
  "Format for modifystamps (c.f. 'format-time-string').")
(defcustom modifystamp-prefix "MODIFYSTAMP(("
  "String identifying start of modifystamp.")
(defcustom modifystamp-suffix "))"
  "String that terminates a modifystamp.")
(defun update-modifystamps ()
  "Find modifystamps and replace them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(let ((regexp (concat "^"
			(regexp-quote modifystamp-prefix)
			"\\(.*\\)"
			(regexp-quote modifystamp-suffix)
			"$")))
	(while (re-search-forward regexp nil t)
	  (replace-match (format-time-string modifystamp-format
					     last-change-time)
			 t t nil 1))))))
  (setq last-change-time nil)
  nil)
(add-hook 'first-change-hook 'update-modifystamps nil t)

(defun maybe-update-modifystamps ()
  "Call 'update-modifystamps' if the buffer has been modified."
  (if last-change-time
      (update-modifystamps)))
(add-hook 'before-save-hook 'maybe-update-modifystamps)
;(remove-hook 'before-save-hook 'maybe-update-modifystamps)  

(make-local-variable 'after-change-functions)

(defcustom last-change-time nil
  "Time of last buffer modification.")
(make-variable-buffer-local 'last-change-time)
(add-hook 'after-change-functions 'remember-change-time nil t)

(defun remember-change-time (&rest unused)
  "Store the current time in 'last-change-time'."
  (setq last-change-time (current-time)))

;; (add-hook 'before-save-hook
;; 	  '(lambda ()
;; 	     (if last-change-time
;; 		 (update-modifystamps last-change-time))))


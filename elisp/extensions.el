;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples from Writing GNU Emacs Extensions
;;
;; We won't bother with changing help-command
;; binding, since in Mac DEL key works fine.
;;
;; (global-set-key (kbd "\M-?") 'help-command) 
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

(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))

(global-set-key (kbd "\M-,") 'point-to-top)

(defun point-to-bottom ()
  "Put point at beginning of last visible line."
  (interactive)
  (move-to-window-line -1))

(global-set-key (kbd "\M-.") 'point-to-bottom)

(defun line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))

(global-set-key (kbd "\M-!") 'line-to-top)


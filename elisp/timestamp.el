;; From Writing GNU Emacs Extenstions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 4. Searching and Modifying Buffers
;; 
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
(defcustom writestamp-format "%x %X"
  "Format for writestamps (c.f. 'format-time-string').")
(defcustom writestamp-prefix "WRITESTAMP(("
  "Unique string identifying start of writestamp.")
(defcustom writestamp-suffix "))"
   "String that terminates a writestamp.")

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

(defcustom last-change-time nil
  "Time of last buffer modification.")


(defun remember-change-time (&rest unused)
  "Store the current time in 'last-change-time'."
  (setq last-change-time (current-time)))

(defcustom modifystamp-format "%x %X"
  "Format for modifystamps (c.f. 'format-time-string').")
(defcustom modifystamp-prefix "MODIFYSTAMP(("
  "String identifying start of modifystamp.")
(defcustom modifystamp-suffix "))"
  "String that terminates a modifystamp.")

(defun maybe-update-modifystamps ()
  "Call 'update-modifystamps' if the buffer has been modified."
  (if last-change-time
      (update-modifystamps last-change-time)))


(defun update-modifystamps (time)
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
					     time)
			 t t nil 1))))))
  (setq last-change-time nil)
  nil)

(make-variable-buffer-local 'last-change-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks removed per text

;; (add-hook 'before-save-hook 'update-writestamps)



;; (add-hook 'before-save-hook 'maybe-update-modifystamps)


;; (make-local-variable 'after-change-functions)


;; (add-hook 'after-change-functions 'remember-change-time nil t)


;; (add-hook 'before-save-hook
;;  	  '(lambda ()
;;  	     (if last-change-time
;; 		   (update-modifystamps last-change-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only for first versin of modifystamps -- don't use
;; (make-local-variable 'first-change-hook)
;; (add-hook 'first-change-hook 'update-modifystamps nil t)

(provide 'timestamp)

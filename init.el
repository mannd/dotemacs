;;; init.el --- Initialization file for Emacs

;;; Author: David Mann

;;; Commentary:

;; Most of the actual initing happens in ~/.emacs.d/configuration.org

;;; Code:

;; Time Emacs startup.

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Load my literate Emacs configuration.
(org-babel-load-file "~/.emacs.d/configuration.org")

;; Finish timing the startup.
;; THIS NEEDS TO BE LAST IN init.el.
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
	    `(lambda ()
	       (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
		 (message "Loading %s...done (%.3fs) [after-init]" ,load-file-name elapsed))) t))

;;; init end here

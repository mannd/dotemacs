;;; init.el --- Initialization file for Emacs

;; Author: David Mann

;;; Commentary:

;; This is the Emacs “init file after declaring .emacs bandkruptcy.
;; It is being converted to a literate programming format.  Stay tuned.

;;; Code:

;; Time Emacs startup.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Load my literate Emacs configuration.
(org-babel-load-file "~/.emacs.d/configuration.org")

;; Finish timing the startup.
;; THIS NEEDS TO BE LAST IN init.el.
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;;; init ends here

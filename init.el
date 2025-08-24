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
;; (let* ((org-file "~/.emacs.d/configuration.org")
;;        (el-file  "~/.emacs.d/configuration.el")
;;        (elc-file "~/.emacs.d/configuration.elc"))
;;   ;; Tangle if .org is newer than .el
;;   (when (file-newer-than-file-p org-file el-file)
;;     (require 'org)
;;     (org-babel-tangle-file org-file el-file))

;;   ;; Byte-compile if .el is newer than .elc
;;   (when (file-newer-than-file-p el-file elc-file)
;;     (byte-compile-file el-file))

;;   ;; Load the compiled version if it exists, otherwise the plain .el
;;   (if (file-exists-p elc-file)
;;       (load elc-file)
;;     (load el-file)))


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

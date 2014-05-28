;; for now will load the aquamacs Preferences.el file, but only
;; if this is not Aquamacs.
(if (not (boundp 'aquamacs-version))
    (load "~/Library/Preferences/Aquamacs Emacs/Preferences.el"))

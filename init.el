;; This is the Emacs init file (= .emacs)
;; ~/.emacs.d/init.el
;; David Mann
;; new init file for latest emacs, latest org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Re: Aquamacs
;; Aquamacs doesn't load ~/.emacs.d/init.el, just .emacs, which in
;; my configuration is empty.  Thus in the Aquamacs Preferences.el
;; file there are these lines:
;;(add-to-list 'load-path "~/.emacs.d/")
;;(load-library "init")
;; Thus some stuff is skipped here if using Aquamacs,
;; so we use this function to see which emacs we are using.
(defun is-aquamacs ()
  "You are running Aquamacs."
  (interactive)
  (if (boundp 'aquamacs-version) t nil))
;;
(package-initialize nil)
;; org mode
;; don't need org package, we're using the git version
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))
(package-initialize t)
(require 'org)
;; potentially use org-babel for init file at some point
(require 'ob-tangle)
;; set up package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;
;; my elisp files
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "extensions")
;;
;; Standard org key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
;;
;; I like visual-line-mode as default for text, org
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(setq org-hide-leading-stars 'hidestars) ; just one star visible
;;
;; For mobile org
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/todo.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files (list "~/org/todo.org"))
;;
;; notes directory
(setq org-default-notes-file (concat org-directory "/" "organizer.org"))
;;
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; but we'll keep menu-bar-mode, since it's up there anyway
;; eliminate splash screen
(setq inhibit-splash-screen t)
;; No backup files
(setq make-backup-files nil)
;; Show column number
(column-number-mode t)
;;
;; save desktop
;;(desktop-save-mode 1)
;; Let's load a nice color-theme
;; we'll use a dark theme to distinguish emacs-carbon from aquamacs
(if (not (is-aquamacs))
	 (load-theme 'tsdh-light t))
;;
;; prevent graphical dialogs which crash emacs in Mac OS, (but
;; not with Aquamacs)
(if (not (is-aquamacs))
    (setq use-dialog-box nil))
;; use visible-bell
(setq visible-bell t)
;; save history
(savehist-mode t)
;;
;; provide shortcut register to this file
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?t '(file . "~/org/todo.org"))
;;
;; set up path for eshell and term
(setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 (getenv "PATH")))
;; set up term and eshell to use emacsclient as default EDITOR
(setenv "EDITOR" "emacsclient")
(setenv "ALTERNATIVE_EDITOR" "emacs")
(setenv "VISUAL" "emacsclient")
;; set up emacs as server
(server-start)
;;

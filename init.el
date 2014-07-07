;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el
;; This is the Emacs init file after declaring .emacs bandkruptcy
;; David Mann
;; 
;; Re: Aquamacs
;; Aquamacs doesn't load ~/.emacs.d/init.el, just .emacs, which in
;; my configuration is empty.  Thus in the Aquamacs Preferences.el
;; file there are these lines:
;; (add-to-list 'load-path "~/.emacs.d/")
;; (load-library "init")
;; Thus some stuff is skipped here if using Aquamacs,
;; so we use this function to see which emacs we are using.
(defun is-aquamacs ()
  "You are running Aquamacs."
  (interactive)
  (if (boundp 'aquamacs-version) t nil))
;;
;; set up org mode
;; don't need org package, we're using the git version
;;
(defvar my-git-directory "~/git")
;;
(package-initialize nil)
(add-to-list 'load-path (expand-file-name (concat my-git-directory "/org-mode/lisp")))
(add-to-list 'load-path (expand-file-name (concat my-git-directory "/org-mode/contrib/lisp")))
;; I use org-mode for txt files too
(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))
(package-initialize t)
(require 'org)
;; potentially use org-babel for init file at some point
(require 'ob-tangle)
;;
;; set up package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; use-package must be loaded by package system
(require 'use-package)
;;
;; my elisp files from "Writing GNU Emacs Extensions" and others?
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "extensions")
;;
;; Standard org key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
;; ~/org is a symlink to "~/Dropbox/org"
(setq org-directory "~/org")
;;
;; I like visual-line-mode as default for text, org
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
;; tweak the stars
(setq org-hide-leading-stars 'hidestars) ; just one star visible
;;
;; agenda files
(setq org-agenda-files '((concat org-directory "/inbox.org")
			 (concat org-directory "/home.org")
			 (concat org-directory "/epstudios.org")
			 (concat org-directory "/family.org")))
;;
;; For mobile org
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files (list (concat org-directory "/inbox.org") 
			     (concat org-directory "/home.org") 
			     (concat org-directory "/epstudios.org")
			     (concat org-directory "/family.org")))
;;
;; refile targets
(setq org-refile-targets
      '((nil :maxlevel . 4)
	(org-agenda-files :maxlevel . 4)
	((concat org-directory "/org.org") :maxlevel . 4)))
;;
;; default tasks/notes/inbox file
(setq org-default-notes-file (concat org-directory "/inbox.org"))
;;
;; Capture templates
(setq org-capture-templates 
      '(("t" "todo" entry (file+headline (concat org-directory "/inbox.org") "Tasks") 
	 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	("n" "note" entry (file+headline (concat org-directory "/inbox.org") "Notes") 
	 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)))
;;
;; experiment with more TODO states
(setq org-todo-keywords
      '((sequence
	 "TODO(t)"
	 "STARTED(s)"
	 "WAITING(w@/!)"
	 "POSTPONED(p)"
	 "SOMEDAY(s@/!)"
	 "|" "DONE(x!)" "CANCELLED(c@)")))
;;
;; log stuff into drawer
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)
;;
;; screen stuff
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
;; prevent graphical dialogs which crash emacs in Mac OS, (but
;; not with Aquamacs)
(if (not (is-aquamacs))
    (setq use-dialog-box nil))
;; use visible-bell
(setq visible-bell t)
;; save history
(savehist-mode t)
;;
;; provide shortcut registers to files
(set-register ?e '(file . "~/.emacs.d/init.el")) ; e stands for .emacs
(set-register ?i '(file . (concat org-directory "/inbox.org")))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Xiki/ruby stuff
;; set up xiki
;;
;; Use rvm to manage ruby versions
(add-to-list 'load-path "~/.emacs.d/rvm/")
(require 'rvm)
(rvm-use-default)
;;
;; If you want to play with Xiki, go
;; to ~/.emacs.d/elisp/start-xiki.el
;; and M-x eval-buffer
;; 
;; a reasonable color theme
(load-theme 'tsdh-light t)
;; IRC
(use-package erc
	     :config
	     (setq erc-autojoin-channels-alist '(("freenode.net"
						  "#org-mode"
						  "#emacs"))))

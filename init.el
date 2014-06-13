;; This is the Emacs init file (= .emacs)
;; ~/.emacs.d/init.el
;; David Mann
;; new init file for latest emacs, latest org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; I used to keep this in the Aquamacs preference file and
;; had Aquamacs skip this, but it's better to keep it in init.el.
;; Note that Aquamacs customizations override init.el.
;; Thus some stuff is skipped here if using Aquamacs
;;(if (not (boundp 'aquamacs-version))
;;    (load "~/Library/Preferences/Aquamacs Emacs/Preferences.el"))
(defun is-aquamacs ()
  "You are running Aquamacs."
  (interactive)
  (if (boundp 'aquamacs-version) t nil))

(require 'package)
(package-initialize)
;; don't need org package, we're using the git version
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;
;;
;; org mode
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))
(require 'org)
;;
;; my elisp files
;; (load "~/elisp/extensions")
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
;; no longer necessary to turn on font-lock, is default in emacs 24
;;(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-hide-leading-stars 'hidestars) ; just one star visible
;;
;; For mobile org
(setq org-directory "~")
(setq org-mobile-inbox-for-pull "~/todo.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;;
;; Personalization
(setq user-mail-address "mannd@epstudiossoftware.com")
;;
;; Some necessities from old .emacs
;; Lose menus, toolbars, scrollbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
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
	 (load-theme 'wombat t))
;;
;; use visible-bell
(setq visible-bell t)
;; save history
(savehist-mode t)
;;
;; provide shortcut register to this file
(set-register ?e '(file . "~/.emacs.d/init.el"))
;;
;; set up path for eshell
(setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 (getenv "PATH")))
(setenv "EDITOR" "emacsclient")
;; set up emacs as server
(server-start)

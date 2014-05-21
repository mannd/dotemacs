;;
;; ~/.emacs.d/init.el
;; David Mann
;; new init file for latest emacs, latest org-mode
;; 
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;;
;; org mode
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

;; Standard org key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
(add-hook 'org-mode-hook 'turn-on-font-lock)
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
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; eliminate splash screen
(setq inhibit-splash-screen t)
;; No backup files
(setq make-backup-files nil)
;; Show column number
(column-number-mode t)
;;
;; save desktop
(desktop-save-mode 1)
;; Let's load a nice color-theme
;;(load-theme 'tsdh-light t)
(load-theme 'light-blue t)

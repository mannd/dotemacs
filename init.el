;;
;; new init file for latest emacs, latest org-mode
;; org mode
;;
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-hide-leading-stars 'hidestars) ; just one star visible
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
;;
;; Let's load a nice color-theme
(load-theme 'tsdh-light t)

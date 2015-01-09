;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/init.el
;; This is the Emacs init file after declaring .emacs bandkruptcy
;; David Mann
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Re: Aquamacs
;; Aquamacs doesn't load ~/.emacs.d/init.el, just .emacs, which in
;; my configuration is empty.  Thus in the Aquamacs Preferences.el
;; file there are these lines:
;;    (add-to-list 'load-path "~/.emacs.d/")
;;    (load-library "init")
;;
;; There is some stuff that is broken in non-Aquamacs, and right now
;; I am avoiding Aquamacs, so need to test for Aquamacs.
(defun is-aquamacs ()
  "t if you are running Aquamacs."
  (interactive)
  (if (boundp 'aquamacs-version) t nil))
;;
;; set up org mode
;; don't need org package, we're using the git version
(defvar my-git-directory "~/git")
;; use latest org-mode documentation
;; seems like this needs to come early, or is overriden by Info-directory-list
(add-to-list 'Info-default-directory-list
	     (expand-file-name
	      (concat my-git-directory "/org-mode/doc")))
;;
(package-initialize nil)
(add-to-list 'load-path
	     (expand-file-name
	      (concat my-git-directory "/org-mode/lisp")))
(add-to-list 'load-path
	     (expand-file-name
	      (concat my-git-directory "/org-mode/contrib/lisp")) t)
(package-initialize t)
;; prevent loading packages twice after init.el is done
(setq package-enable-at-startup nil)
;;
(require 'org)
;; potentially use org-babel for init file at some point
(require 'ob-tangle)
;; I use org-mode for txt files too
(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))
;;
;; set up package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; use-package must be loaded by package system
(require 'use-package)
;;
;; my elisp files from "Writing GNU Emacs Extensions" and others?
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "extensions")
;; the modifystamp and writestamp stuff in Chapt 4 of above
(require 'timestamp)
;; evernote-mode - note requires ruby 1.9.3
(setq evernote-ruby-command "/Users/mannd/.rvm/rubies/ruby-1.9.3-p547/bin/ruby")
(require 'evernote-mode)
(setq evernote-username "manndmd@gmail.com")
(setq exec-path (cons "/usr/local/bin" exec-path))
(require 'w3m)
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)
;; org-evernote can dump evernote notes into org format
(require 'org-evernote)
;;
;; org-mode setup
;; Standard org key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
;; ~/org is a symlink to "~/Dropbox/org" for easy sharing
(setq org-directory "~/org")
;;
;; I like visual-line-mode as default for text, org
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
;; we hide stars by default even in buffers where we turn off
;; org-indent-mode.
(setq org-hide-leading-stars 'hidestars) ; just one star visible
;; org-indent-mode will be default, turn it off per file as needed
;; with #_STARTUP: noindent
(setq org-startup-indented t)
;;
;; agenda files
(setq org-agenda-files '("~/org/inbox.org"
			 "~/org/personal.org"
			 "~/org/home.org"
			 "~/org/epstudios.org"
			 "~/org/family.org"
			 "~/org/org.org"))
;;
;; For mobile org
(setq org-mobile-inbox-for-pull "~/org/index.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files org-agenda-files)
;;
;; refile targets
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 2)))
;;
;; default tasks/notes/inbox file
(setq org-default-notes-file "~/org/inbox.org")
;;
;; stuck project tweak: projects are level 2 headlines, lacking NEXT action
(setq org-stuck-projects '("+LEVEL=2/-DONE" ("NEXT") nil ""))
;; Capture templates
(setq org-capture-templates 
      '(("t" "todo" entry (file+headline "~/org/inbox.org" "Tasks") 
	 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	("n" "note" entry (file+headline "~/org/inbox.org" "Notes") 
	 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)))
;;
;; experiment with more TODO states
(setq org-todo-keywords
     (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s@/!)" "|" "CANCELLED(c@/!)"))))
;; we'll try making the colors prettier too
(setq org-todo-keyword-faces
      (quote (("NEXT" :foreground "blue" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("HOLD" :foreground "magenta" :weight bold)
	      ("CANCELLED" :foreground "forest green" :weight bold)
	      ("SOMEDAY" :foreground "cyan" :weight bold))))

;; activate org mode speed commands
(setq org-use-speed-commands t)
;;
;; log stuff into drawer
(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)
;;
;; avoid blank lines in org files
(setq org-cycle-separator-lines 0)
;;
;; supress footer in org html export files
(setq org-html-postamble nil)
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
(set-register ?i '(file . "~/org/inbox.org"))
;;
;; set up path for eshell
(setenv "PATH"
	(concat
	 "/Users/mannd/bin" ":"
	 (getenv "PATH")))
(setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 (getenv "PATH")))
;; need this so that emacs finds latex programs
(setenv "PATH"
	(concat
	 "/usr/texbin" ":"
	 (getenv "PATH")))
;; use git in /usr/local/git/bin for eshell
(setenv "PATH"
	(concat
	 "/usr/local/git/bin" ":"
	 (getenv "PATH")))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem with emacsclient was invoking wrong emacsclient (/usr/bin/emacsclient)
(setenv "EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_5/emacsclient")
;; set up emacs as server
(server-start)
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
;;
;;
(add-to-list 'load-path (expand-file-name (concat my-git-directory "/markdown-mode")))
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . grm-mode))
;;
;; multiple cursors (package installed)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-C") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;
;; experimentally put some code in an org-babel file
(org-babel-load-file "~/.emacs.d/dem.org")
;; Magit
(global-set-key (kbd "M-m") 'magit-status)
;; ispell
(setq ispell-program-name "/usr/local/bin/ispell")
;; stuff below added by Custom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evernote-developer-token
   "S=s70:U=79f43a:E=14e6a93ed8b:C=14712e2c020:P=1cd:A=en-devtoken:V=2:H=d547691e1d7dec6c08951f34d37b660b")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

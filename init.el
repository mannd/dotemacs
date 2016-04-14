;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/init.el
;; This is the Emacs init file after declaring .emacs bandkruptcy
;; David Mann
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Set up documenation
;; seems like this needs to come early, or is overriden by Info-directory-list
(add-to-list 'Info-default-directory-list "~/git/org-mode/doc")
(add-to-list 'Info-default-directory-list "~/.emacs.d/elisp/bbdb-3.1.2/doc")

;; override build-in org
(package-initialize nil)
(add-to-list 'load-path "~/git/org-mode/lisp")
(add-to-list 'load-path "~/git/org-mode/contrib/lisp")
(add-to-list 'load-path "~/.emacs.d/elisp/")
(package-initialize t)

;; prevent loading packages twice after init.el is done
(setq package-enable-at-startup nil)

;; set up package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; use-package setup
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; set up org-mode
(require 'org)
(require 'org-checklist)
(require 'ob-tangle)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (js . t)
   (java . t)))
;; file types for org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\|txt\\)$" . org-mode))

;; my elisp files from "Writing GNU Emacs Extensions"
(use-package extensions)
(use-package timestamp)
;; experimentally put some code in an org-babel file
(org-babel-load-file "~/.emacs.d/dem.org")

;; org-mode setup
;; Standard org key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-include-diary t)
;; ~/org is a symlink to "~/Dropbox/org" for easy sharing
(setq org-directory "~/org")
;; I like visual-line-mode as default for text, org
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
;; we hide stars by default even in buffers where we turn off
(setq org-hide-leading-stars 'hidestars) ; just one star visible
(setq org-startup-indented t)

;; agenda files
(setq org-agenda-files '("~/org/inbox.org"
			 "~/org/personal.org"
			 "~/org/home.org"
			 "~/org/epstudios.org"
			 "~/org/family.org"
			 "~/org/org.org"))

;; change default iCalendar target (org.ics conflicts with org.org file)
(setq org-icalendar-combined-agenda-file "~/org/org-calendar.ics")
(setq org-icalendar-include-todo t)

;; For mobile org
(setq org-mobile-inbox-for-pull "~/org/index.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files org-agenda-files)

;; refile targets
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 2)))

;; default tasks/notes/inbox file
(setq org-default-notes-file "~/org/inbox.org")

;; stuck project tweak: projects are level 2 headlines, lacking NEXT action
(setq org-stuck-projects '("+LEVEL=2/-DONE" ("NEXT") nil ""))
;; Capture templates
(setq org-capture-templates 
      '(("t" "todo" entry (file+headline "~/org/inbox.org" "Tasks") 
	 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	("n" "note" entry (file+headline "~/org/inbox.org" "Notes") 
	 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)))

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

;; log stuff into drawer
(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)

;; avoid blank lines in org files
(setq org-cycle-separator-lines 0)

;; supress footer in org html export files
(setq org-html-postamble nil)

;; for lein
(setq exec-path (cons "/Users/mannd/bin" exec-path))

;; evernote-mode - note requires ruby 1.9.3
(use-package evernote-mode
  :disabled t
  :config
  (setq evernote-ruby-command "/Users/mannd/.rvm/rubies/ruby-1.9.3-p547/bin/ruby")
  (setq evernote-username "manndmd@gmail.com")
  (setq exec-path (cons "/usr/local/bin" exec-path))
  (setq exec-path (cons "/usr/local/opt/coreutils/libexec/gnubin" exec-path))
  (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
  (global-set-key "\C-cec" #'evernote-create-note)
  (global-set-key "\C-ceo" 'evernote-open-note)
  (global-set-key "\C-ces" 'evernote-search-notes)
  (global-set-key "\C-ceS" 'evernote-do-saved-search)
  (global-set-key "\C-cew" 'evernote-write-note)
  (global-set-key "\C-cep" 'evernote-post-region)
  (global-set-key "\C-ceb" 'evernote-browser)
  (use-package org-evernote))


;; screen stuff
;; just-one-space makes deletion better
(setq just-one-space t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; but we'll keep menu-bar-mode, since it's up there anyway
;; eliminate splash screen
(setq inhibit-splash-screen t)
;; inhibit scratch message
(setq initial-scratch-message "")
;; No backup files
(setq make-backup-files nil)
;; auto-revert-mode reloads buffer if file changes on disk
(global-auto-revert-mode t)
;; Show column number
(column-number-mode t)
;; Go ahead and ring the silent bell!
(setq visible-bell t)
(setq ring-bell-function t)
;; save history
(savehist-mode t)

;; provide shortcut registers to files
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?i '(file . "~/org/inbox.org"))
(set-register ?g '(file . "~/.emacs.d/gnus.el"))

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
(setenv "PATH"
	(concat
	 "/usr/local/opt/coreutils/libexec/gnubin" ":"
	 (getenv "PATH")))

;; problem with emacsclient was invoking wrong emacsclient
;; (/usr/bin/emacsclient)
;; make sure the emacslient appropriate to the Emacs I am using is used
(setenv "EDITOR" (expand-file-name "bin/emacsclient" invocation-directory))
;; "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_5/emacsclient")

;; set up emacs as server
(require 'server)
(unless (server-running-p)
  (server-start))

;; set up xiki
;; Use rvm to manage ruby versions
(use-package rvm
  :disabled t
  :load-path "~/.emacs.d/rvm/"
  :config 
  (rvm-use-default))
;; If you want to play with Xiki, go
;; to ~/.emacs.d/elisp/start-xiki.el
;; and M-x eval-buffer

;; pick a theme
(load-theme 'tsdh-light t)
;;(load-theme 'wombat t)
;;(load-theme 'leuven t)

;; IRC
(use-package erc
	     :config
	     (setq erc-autojoin-channels-alist '((".*\\.freenode.net"
						  "#org-mode"
						  "#emacs"
						  "#android"
						  "#android-dev"))))

(use-package markdown-mode
  :load-path "~/git/markdown-mode"
  :mode
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode))

;; multiple cursors (package installed)
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-C") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Magit
(use-package magit
  :load-path "~/git/magit/lisp"
  :init
  (use-package magit-gitflow
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
  (global-set-key (kbd "C-x g") 'magit-status)  
  :config 
  (add-to-list 'Info-directory-list
	       "~/git/magit/Documentation/"))


;; ispell
(setq ispell-program-name "/usr/local/bin/ispell")

;; gnus
(setq gnus-init-file "~/.emacs.d/gnus")
(require 'nnir)
;; quit gnus automatically on exit emacs
(defadvice save-buffers-kill-emacs (before rgb/gnus-exit)
(gnus-group-exit))
(add-hook 'gnus-started-hook
(lambda () (ad-activate 'save-buffers-kill-emacs)))
(add-hook 'gnus-after-exiting-gnus-hook
(lambda () (ad-deactivate 'save-buffers-kill-emacs)))

;; use-package testing
(use-package olivetti :ensure t :defer t)
(use-package htmlize :ensure t :defer t)
(use-package cider :ensure t :defer t)
(use-package w3m :ensure t :defer t)

;; stuff below added by Custom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "f2adf01a9191e9787b0182f97eae18d118ae43d9")
 '(evernote-developer-token
   "S=s70:U=79f43a:E=14e6a93ed8b:C=14712e2c020:P=1cd:A=en-devtoken:V=2:H=d547691e1d7dec6c08951f34d37b660b")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (lein htmlize dracula-theme fountain-mode js3-mode js2-mode writeroom-mode use-package tagedit swift-mode smex rainbow-delimiters paredit multiple-cursors geiser exec-path-from-shell debbugs color-theme clojure-mode-extra-font-locking bbdb-vcard bbdb-csv-import)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure stuff taken from https://github.com/flyingmachine/emacs-for-clojure/blob/master/init.el

(defvar clojure-packages
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    smex
    rainbow-delimiters
    tagedit
    ))					

(dolist (p clojure-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; helm
;;(add-to-list 'load-path "~/git/emacs-async")
(use-package helm-config
  :demand t
  :load-path "~/git/helm"
  :config
  (use-package helm-mode
    :init
    (helm-mode 1)))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)

;; longitude latitude for sunset/sunrise
;; Paris, FR
;; (setq calendar-latitude 48.9)
;; (setq calendar-longitude 2.5)
;; (setq calendar-location-name "Paris, FR")
;; Parker, CO
(setq calendar-latitude 39.4868360)
(setq calendar-longitude -104.7450340)
(setq calendar-location-name "Parker, CO")

;; BBDB v3
(require 'bbdb-loaddefs "~/.emacs.d/elisp/bbdb-3.1.2/lisp/bbdb-loaddefs.el")
(require 'bbdb)
(setq bbdb-print-text-path "~/.emacs.d/elisp/bbdb-3.1.2")
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)
(setq bbdb-mua-pop-up-window-size 0.1)
(setq bbdb-mua-update-interactive-p '(query . create))
(setq bbdb-message-all-addresses t)
(add-hook
 'gnus-summary-mode-hook
 (lambda ()
   (define-key gnus-summary-mode-map (kbd ";") 'bddb-mua-edit-field)
   ))

;; ledger
(use-package ledger-mode
  :load-path "~/lisp"
  :mode ("\\.ledger$" "\\.dat$"))

;; use 'a' to open in current buffer, not create new buffer in dired
(put 'dired-find-alternate-file 'disabled nil)

;; try less jumpy trackpad scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control))))

;; Proper title capitalization function
;; Now just use Karls Voigt's improved version in ~/.emacs.d/elisp
(use-package title-capitalization)

;; twittering-mode
(use-package twittering-mode
  :config (setq twittering-use-master-password t)
  :load-path "~/git/twittering-mode/")

;; abbrev mode
(setq-default abbrev-mode t)
(setq save-abbrevs t)
(put 'upcase-region 'disabled nil)

;; put time and day in mode-line (good for full screen Emacs)
(setq display-time-day-and-date t)
(display-time-mode t)

;; for mac printing
(add-to-list 'load-path "~/.emacs.d/elisp/mac-print-mode")
(when (require 'mac-print-mode nil t)
  (mac-print-mode 1)
  (global-set-key (kbd "M-p") 'mac-print-buffer))

;; timing
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

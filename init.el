;; ~/.emacs.d/init.el
;; This is the Emacs init file after declaring .emacs bandkruptcy
;; David Mann
;; 
;;; Code:

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; below suppresses flycheck errors
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; Since we are using built-in org, don’t need this work around
;; Set up documenation
;; seems like this needs to come early, or is overriden by Info-directory-list
;; (add-to-list 'Info-default-directory-list "~/git/org-mode/doc")

;; NOTE: now using built-in org
;; override built-in org
(package-initialize)
;; (add-to-list 'load-path "~/git/org-mode/lisp")
;; (add-to-list 'load-path  "~/git/org-mode/contrib/lisp")
(add-to-list 'load-path "~/.emacs.d/elisp/")
;(package-initialize t)

;; prevent loading packages twice after init.el is done
(setq package-enable-at-startup nil)

(setq user-full-name "David Mann, MD"
      user-mail-address "manndmd@gmail.com")

;; set up package sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; use-package setup
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; NOT NEEDED, using built-in org
;; set up org-mode
;; (require 'org)
;; (require 'org-checklist)
;; (require 'ob-tangle)
;; (require 'org-drill)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (js . t)
   (shell . t)
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
(global-set-key "\C-cb" 'org-switchb)
(setq org-agenda-include-diary t)
;; ~/org is a symlink to "~/Dropbox/org" for easy sharing
(setq org-directory "~/org")
;; I like visual-line-mode as default for text, org
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
;; we hide stars by default even in buffers where we turn off
(setq org-hide-leading-stars 'hidestars) ; just one star visible
(setq org-startup-indented t)

;; agenda files
;; Note org-gcal seems to not work with multiple calendars
(setq org-agenda-files '("~/org/inbox.org"
			 "~/org/personal.org"
			 "~/org/home.org"
			 "~/org/epstudios.org"
			 "~/org/family.org"
			 "~/org/org.org"
			 "~/org/calendars/gcal.org"))
			 ;; "~/org/calendars/persgcal.org"
			 ;; "~/org/calendars/gretgcal.org"))

;; custom agenda commands
;; see https://stackoverflow.com/questions/31639086/emacs-org-mode-how-can-i-filter-on-tags-and-todo-status-simultaneously
(setq org-agenda-custom-commands
      '(("p" "Projects" tags "project/TODO" nil)
	("n" "Agenda and TODOs"
	 ((agenda "")
	  (alltodo "")))
	("c" "Agenda and Projects"
	 ((agenda "")
	  (tags "project/TODO"
		((org-agenda-overriding-header "Projects")))))
	))

;; change default iCalendar target (org.ics conflicts with org.org file)
(setq org-icalendar-combined-agenda-file "~/org/org-calendar.ics")
(setq org-icalendar-include-todo t)

;; For mobile org -- I’ve switched to beorg on iOS
;;(setq org-mobile-inbox-for-pull "~/org/index.org")
;;(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;;(setq org-mobile-files org-agenda-files)

;; refile targets
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 2)))

;; default tasks/notes/inbox file
(setq org-default-notes-file "~/org/inbox.org")

;; stuck project tweak: projects are level 2 headlines, lacking NEXT action
(setq org-stuck-projects '("+project/-DONE-CANCELLED" ("NEXT") nil ""))
(setq org-tags-exclude-from-inheritance '("project"))
;; Capture templates
(setq org-capture-templates
      '(("c" "Calendar appointment" entry (file "~/Dropbox/org/gcal.org")
	 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
	("t" "todo" entry (file+headline "~/org/inbox.org" "Tasks")
	 "* TODO %?\n%U\n%a\n")
	("n" "note" entry (file+headline "~/org/inbox.org" "Notes")
	 "* %? :NOTE:\n%U\n%a\n")
	("j" "journal entry"
	 entry (file+olp+datetree "~/Documents/journal.org.gpg")
	 "**** %U %^{Title}\n%?")
	("g" "German vocabulary"
	 entry (file+headline "~/org/german.org" "German")
	 "* <[%^{German word}]> :drill:\n :PROPERTIES:\n    :DRILL_CARD_TYPE: twosided\n    :END:\n** German\n %^{Detailed German word|%\\1}\n** English\n %^{English translation}")
	("f" "French vocabulary"
	 entry (file+headline "~/org/french.org" "French")
	 "* <[%^{French word}]> :drill:\n :PROPERTIES:\n    :DRILL_CARD_TYPE: twosided\n    :END:\n** French\n %^{Detailed French word|%\\1}\n** English\n %^{English translation}")))

;; experiment with more TODO states
(setq org-todo-keywords
     (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "PENDING(p)" "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s@/!)" "|" "CANCELLED(c@/!)"))))

;; we'll try making the colors prettier too
(setq org-todo-keyword-faces
      (quote (("NEXT" :foreground "blue" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("PENDING" :foreground "orange" :weight bold)
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

;; widen margins in Latex export
(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))

;; imenu-list
(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-position 'left))

;; Google calendar sync
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "981452983982-lrd1cmkcrn6jf30k7v87ih24ai1ai2ea.apps.googleusercontent.com"
	org-gcal-client-secret "MiMLtnyy51Sq_RxwBW9rwZMp"
	org-gcal-file-alist '(("manndmd@gmail.com" . "~/Dropbox/org/calendars/gcal.org"))))
			      ;; ("a46egt8krbmcg72csc9vtmgdro@group.calendar.google.com" . "~/Dropbox/org/calendars/persgcal.org")
			      ;; ("manngmd@gmail.com" . "~/Dropbox/org/calendars/gretgcal.org"))))
  
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch) ))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

;; evernote-mode - note requires ruby 1.9.3 (or later??)
;; disabled
(use-package evernote-mode
  :disabled t
  :config
  (setq evernote-ruby-command "/Users/mannd/.rvm/rubies/ruby-1.9.3-p547/bin/ruby")
  (setq evernote-username "manndmd@gmail.com")
  (setq exec-path (cons "/usr/local/bin" exec-path))
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

;; problem with emacsclient was invoking wrong emacsclient
;; (/usr/bin/emacsclient)
;; make sure the emacslient appropriate to the Emacs I am using is used
(setenv "EDITOR" (expand-file-name "bin/emacsclient" invocation-directory))
;; "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_5/emacsclient")

;; set up emacs as server
(require 'server)
(unless (server-running-p)
  (server-start))

;; encryption
(require 'epa-file)
(epa-file-enable)
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
;;(load-theme 'tsdh-light t)
;;(load-theme 'wombat t)
;;(load-theme 'leuven t)
;;(load-theme 'dracula t)
;;(load-theme 'light-blue t)
(load-theme 'leuven t)

;; IRC
(use-package erc
	     :config
	     (setq erc-autojoin-channels-alist '((".*\\.freenode.net"
						  "#org-mode"
						  "#emacs"
						  "#android"
						  "#android-dev"))))

;; markdown-mode
(use-package markdown-mode
  :load-path "~/git/markdown-mode"
  :mode (("README\\.md\\'" . gfm-mode)
  ("README\\.markdown\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  (electric-quote-mode -1))

  
;; multiple cursors (package installed)
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-C") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Magit
(use-package magit
;  :disabed t
  :load-path "~/git/magit/lisp"
  :init
  (use-package magit-gitflow
;    :disabled t
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
  (use-package with-editor
    :load-path "~/git/with-editor")
  (global-set-key (kbd "C-x g") 'magit-status)
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-additional-directory-list
  	       "~/git/magit/Documentation/")))

;; Magithub
(use-package magithub
  :disabled t
  :load-path "~/git/magithub"
  :after magit
  :config (magithub-feature-autoinject t))

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
;; (defun my-reset-gmail-server()
  ;; (gnus-server-close-server "nnimap:gmail"))
;; (add-hook 'gnus-get-new-news-hook
	  ;; 'my-reset-gmail-server)


;; change gnus gmail links to All Mail links when tasks
;; must use org-capture for this to work
(defun dem/replace()
  (interactive)
  (goto-char 1)
  (setq-local search-invisible t)
  (replace-string "gnus:INBOX" "gnus:%5BGmail%5D/All%20Mail"))

(add-hook 'org-capture-prepare-finalize-hook 'dem/replace)

;; some use-package stuff
(use-package olivetti :ensure t :defer t)
(use-package htmlize :ensure t :defer t)
(use-package cider :ensure t :defer t)
(use-package w3m :ensure t :defer t)

;; customizations in its own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
    (helm-mode 1))
  :init
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'helm-M-x))

;; longitude latitude for sunset/sunrise
;; Paris, FR
;; (setq calendar-latitude 48.9)
;; (setq calendar-longitude 2.5)
;; (setq calendar-location-name "Paris, FR")
;; Parker, CO
(setq calendar-latitude 39.4868360)
(setq calendar-longitude -104.7450340)
(setq calendar-location-name "Parker, CO")

;; for forecast-mode, darksky.net api key
(use-package forecast
  :demand t
  :config
  ;; darksky.net api key
  (setq forecast-api-key "1806e2e569afcd58feb6a8568e0857ba"))

;; try calfw calendar
(use-package calfw
  :load-path "~/git/emacs-calfw"
  :init
  (use-package calfw-org))

;; BBDB v3
(use-package bbdb-loaddefs
  :load-path "~/.emacs.d/elisp/bbdb-3.1.2/lisp/"
  :init
  (use-package bbdb)
  (add-hook
 'gnus-summary-mode-hook
 (lambda ()
   (define-key gnus-summary-mode-map (kbd ";") 'bddb-mua-edit-field)
   ))
  :config
  (setq bbdb-print-text-path "~/.emacs.d/elisp/bbdb-3.1.2")
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq bbdb-mua-pop-up-window-size 0.1)
  (setq bbdb-mua-update-interactive-p '(query . create))
  (setq bbdb-message-all-addresses t))

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

;; C-sharp mode
(use-package csharp-mode)

;; twittering-mode
(use-package twittering-mode
;  :disabled t
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
  ;; (global-set-key (kbd "M-p") 'mac-print-buffer))
)
;; Zen-burn
(use-package zenburn-theme
  :disabled nil
  :ensure t
  :config
  (load-theme 'zenburn t t))		;not enabled

(if (eq system-type 'windows-nt)
(progn
  (info-initialize)
  (add-to-list 'Info-additional-directory-list "~/git/org-mode/doc")
  (add-to-list 'Info-additional-directory-list "C:/Users/mannd/bin/emacs-24.5-bin-i686-mingw32/share/info")
  (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-12"))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))))

;; Hydra
(use-package hydra
  :disabled t
  :load-path "~/git/hydra")

;; (defhydra hyrdra-zoom (global-map "<f2>")
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))

;; iBuffer is better
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Common Lisp
(setq inferior-lisp-program "clisp")

;; xcode-mode -- doesn't work with Xcode 8 yet
;; (use-package xcode-mode
;;     :load-path "~/git/xcode-mode"
;;    :ensure t)

;; new and improved way to make PATH and exec-path match bash config
;; exec-path-from-shell doesn't like using .bashrc for .bash_profile,
;; it looks like it would prefer using .profile, so below I
;; disable the error message that occurs.
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


;; this package needed to make rvm work in Emacs
(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

;; compile buffer scrolls
(setq compilation-scroll-output t)

;; yasnippet
(use-package yasnippet
  :load-path "~/git/yasnippet"
  :config
  (yas-global-mode 1))

;; company-sourcekit for Swift programming
(use-package company-sourcekit
  :ensure t
  :config
  (add-to-list 'company-backends 'company-sourcekit))

;; xcode documentation -- Doesn't work
;; (use-package xcode-document-viewer
;;   :load-path "~/git/emacs-xcode-document-viewer"
;;   :init
;;   (use-package anything
;;     :ensure t)
;;   :config
;;   (setq xcdoc:document-path "/Applications/Xcode.app/Contents/Developer/Documentation/DocSets/com.apple.adc.documentation.docset")
;;   (setq xcdoc:open-w3m-other-buffer t))

;; flycheck
;; note that flycheck C-c ! conflicts with org-mode
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (add-to-list 'flycheck-checkers 'swift)
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
  (define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive))

;; fix for yas-snippet breaking term-mode TABS
;; see https://github.com/joaotavora/yasnippet/issues/289
(add-hook 'term-mode-hook (lambda()
			    (yas-minor-mode -1)))

;; swift-mode to use company-mode by default
(add-hook 'swift-mode-hook (lambda()
			     (company-mode t)))

;; flycheck-swift
;; (use-package flycheck-swift
;;   :load-path "~/git/flycheck-swift"
;;   :config
;;   (eval-after-load 'flycheck '(flycheck-swift-setup)))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; send deleted files to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

;; try improving scrolling with trackpad
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil)))

;; auctex
(use-package tex-mode
  :ensure t)

;; graphviz dot mode
;; seems broken in emacs 26 master branch for now
(use-package graphviz-dot-mode
  :disabled t
  :ensure t)

;; play with evil mode
(use-package evil
  :ensure t
  :init
  ;; c-u in evil-mode works like in vim (page up)
  ;; must be set before package is loaded
  (setq evil-want-C-u-scroll t)
  :config
  ;; Make movement keys work respect visual lines
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (setq evil-search-module 'evil-search)
  (setq-default evil-cross-lines t)
  ;; git commit buffers start in insert mode
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'cider-repl 'emacs)
  (evil-set-initial-state 'cider-error 'emacs)
  (add-to-list 'evil-emacs-state-modes 'forecast-mode)
  (setq-default evil-cross-lines t))

;; use evil-magit to match tags
(use-package evil-matchit
  :ensure t
  :init
  (global-evil-matchit-mode 1))

;; figure out if .h files are C or Objective C
;; (add-to-list 'magic-mode-alist
;; 	     `(,(lambda ()
;; 		  (and (string= (file-name-extension buffer-file-name) "h")
;; 		       (re-search-forward "@\\<interface\\>"
;; 					  magic-mode-regexp-match-limit t)))
;; 	       . objc-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timing
;; THIS NEEDS TO BE LAST IN init.el
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

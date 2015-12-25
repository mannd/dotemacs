(setq user-mail-address "manndmd@gmail.com"
      user-full-name "David Mann")

(setq gnus-ignored-newsgroups "")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnir-search-engine imap)))
(add-to-list 'gnus-secondary-select-methods
       '(nnimap "epstudios"
	       (nnimap-address "mail.epstudiossoftware.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnir-search-engine imap)))
(add-to-list 'gnus-secondary-select-methods
	     '(nntp "news.gmane.org"))

(setq gnus-agent nil)


(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
				   "manndmd@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq bbdb-send-mail-style 'gnus)

(setq gnus-move-split-methods
      '((".*" "[Gmail]/All Mail")))

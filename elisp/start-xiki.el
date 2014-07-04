;; M-x eval-buffer to activate xiki, 
;;
;; Make sure you set rvm 1.9.3 first
;;    M-x rvm-use 1.9.3
;; You may need to start xiki in shell running ruby 1.9.3 first
;;
(add-to-list 'load-path "/Users/mannd/.rvm/gems/ruby-1.9.3-p547/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
(require 'el4r)
(el4r-boot)
(el4r-troubleshooting-keys)

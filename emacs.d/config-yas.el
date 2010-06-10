;;/usr/share/emacs/site-lisp/yas")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
;;(yas/load-directory "/usr/share/emacs/site-lisp/yas/snippets")
(yas/load-directory "~/.emacs.d/plugins/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))
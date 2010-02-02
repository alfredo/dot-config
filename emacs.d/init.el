; Load directories
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/plugins")

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))


; ido config
(load-library "config-ido")
; yas config
(load-library "config-yas")
; python mode
(load-library "config-python-mode")
;  themes
(load-library "config-color-themes")
; django 
(load-library "config-django")

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

; tmp scripts to test
;(load-library "config-tmp")
(require 'php-mode)

; backup dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/.emacs-backups")))
; selected area
(setq transient-mark-mode t)
;(setq shift-select-mode t) 

(global-set-key "\C-z" 'undo)
(global-set-key "\C-c\C-i" 'indent-region) ; C-u C-c TAB => (un)indent-region
(global-set-key "\C-c;" 'comment-or-uncomment-region)

(setq fill-column 70)
(column-number-mode 1)
(show-paren-mode t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq default-frame-alist '((font . "terminus")))

; copy and paste
(setq x-select-enable-clipboard t)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)


; never use tabs
(setq-default indent-tabs-mode nil)

; colored
(global-font-lock-mode 1)

; version control
(setq version-control t)
(setq delete-old-versions 1)
(setq kept-new-versions 10)
(setq kept-old-versions 10)
(setq backup-by-copying t)
(add-hook 'before-save-hook 
          '(lambda ()
	     (setq buffer-backed-up nil)))

; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


; unicode 
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)


; config
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; font
(setq default-frame-alist '((font . "terminus")))

(setq fill-column 70)
(column-number-mode 1)
(show-paren-mode t)

; never use tabs
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

; colored
(global-font-lock-mode 1)

; selected area
(setq transient-mark-mode t)

; unicode 
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)

; version control
(setq version-control t)
(setq delete-old-versions 1)
(setq kept-new-versions 10)
(setq kept-old-versions 10)
(setq backup-by-copying t)
(add-hook 'before-save-hook 
          '(lambda ()
	     (setq buffer-backed-up nil)))
; backup dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/.emacs-backups")))

;; full screen
(defun toggle-fullscreen() 
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(toggle-fullscreen)

; desktop
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb";
	        "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


; Load directories
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/plugins")

; key bindings
(load-library "config-key-bindings")
; ido config
(load-library "config-ido")
; yas config
(load-library "config-yas")
; themes
(load-library "config-color-themes")
; html config
(load-library "config-html")
; python mode
(load-library "config-python-mode")
; django
(load-library "config-django")
; shell mode
(load-library "config-shell")
;; extra hooks
(load-library "config-extras")
;; groovy mode
(load-library "config-groovy")
;; processing mode
(load-library "config-processing")

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'psvn)

(require 'ack)

(require 'mic-paren)
(paren-activate)
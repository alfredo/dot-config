(require 'color-theme)
(color-theme-initialize)
;(color-theme-calm-forest)
(color-theme-ld-dark)
(ansi-color-for-comint-mode-on)
(setq inhibit-startup-message t)
(set-default 'indicate-empty-lines nil)
(require 'cursor-chg)
(change-cursor-mode 1)
(change-cursor-mode 1) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle

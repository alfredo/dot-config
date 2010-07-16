(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.fbml$" . html-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
;;(add-to-list 'auto-mode-alist '("\\.sass\\'" . css-mode))

(defun fb ()
  "fix buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))


(require 'rainbow-mode)

;(require 'zencoding-mode)
;(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

; javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
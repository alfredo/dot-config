(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook 'flymake-mode 'python-pep8-mode)

(load-library "flymake-cursor")

(require 'ipython)
(setq python-python-command "ipython")
(setq py-python-command-args '( "-colors" "Linux"))

;; pep 8
(require 'python-pep8)
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")



; bind RET to py-newline-and-indent
;(add-hook 'python-mode-hook '(lambda ()
;     (define-key python-mode-map "\C-m" 'newline-and-indent)))

;(setq ropemacs-enable-shortcuts nil)
;(setq ropemacs-local-prefix "C-c C-p")
;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport 't)

;; virtualenv loading
;;(add-hook 'python-mode-hook '(lambda () (require 'virtualenv)))

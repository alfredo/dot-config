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
(add-hook 'python-mode-hook 'flymake-mode)

(load-library "flymake-cursor")


;;; Electric Pairs
;(add-hook 'python-mode-hook
;     (lambda ()
;      (define-key python-mode-map "(" 'electric-pair)
;      (define-key python-mode-map "[" 'electric-pair)
;      (define-key python-mode-map "{" 'electric-pair)))
;(defun electric-pair ()
;  "Insert character pair without sournding spaces"
;  (interactive)
;  (let (parens-require-spaces)
;    (insert-pair)))


; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))


;; virtualenv loading
(add-hook 'python-mode-hook '(lambda () (require 'virtualenv)))
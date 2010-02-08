(load "django-mode")
(load "django-html-mode")

(defun django-insert-trans-block (from to &optional buffer)
 (interactive "*r")
 (save-excursion
   (save-restriction
     (narrow-to-region from to)
     (goto-char from)
     (iso-iso2sgml from to)
     (insert "{% blocktrans %}")
     (goto-char (point-max))
     (insert "{% endblocktrans %}")
     (point-max))))

(defun django-insert-trans (from to &optional buffer)
 (interactive "*r")
 (save-excursion
   (save-restriction
     (narrow-to-region from to)
     (goto-char from)
     (iso-iso2sgml from to)
     (insert "{% trans \"")
     (goto-char (point-max))
     (insert "\" %}")
     (point-max))))

(add-hook 'sgml-mode-hook
        (lambda ()
          (local-set-key "\C-c\C-g" 'django-insert-trans)
          (setq indent-tabs-mode nil)
          ))
(add-hook 'sgml-mode-hook
        (lambda ()
          (local-set-key "\C-c\C-t" 'django-insert-trans-block)
          (setq indent-tabs-mode nil)
          ))

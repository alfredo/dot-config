;; chmod u+x files that have a shebang line
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
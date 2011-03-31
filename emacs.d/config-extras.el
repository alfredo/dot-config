(defun fixfile ()
  "Fix the suppresion list"
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t)
  (while (re-search-forward "^...$" nil t)
    (replace-match ""))
  (goto-line 1)
  (while (re-search-forward "^..$" nil t)
    (replace-match ""))
  (goto-line 1)
  (while (re-search-forward "^.$" nil t)
    (replace-match ""))
  (goto-line 1)
  (flush-lines "^$")
  )

(require 'pastebin)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg/index.org")

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

(require 'gist)

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
;; SMTP server for mail posting. Default: nil
(setq wl-smtp-posting-server "www.loewyremote.com")

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))


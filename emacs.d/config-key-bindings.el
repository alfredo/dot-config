(global-set-key "\C-z" 'undo)


;(global-set-key "\C-c\C-i" 'indent-region) ; C-u C-c TAB => (un)indent-region

; copy and paste
(setq x-select-enable-clipboard t)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
;(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; nom nom eat white space for next word
(global-set-key (kbd "M-s") 'fixup-whitespace)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c t") 'delete-trailing-whitespace)

(global-set-key (kbd "C-c o") 'occur)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)


;; save your pinky
(defun save-the-pinky-buffer () (interactive) (message "Save your pinky! Use s-b (Opt-b) instead."))
(defun save-the-pinky-open () (interactive) (message "Save your pinky! Use M-o (Cmd-o) instead."))
(defun save-the-pinky-save () (interactive) (message "Save your pinky! Use M-s (Cmd-s) instead."))
(defun save-the-pinky-window () (interactive) (message "Save your pinky! Use M-` (Cmd-`) instead."))
;(global-set-key "\C-x\C-f" 'save-the-pinky-open)
;(global-set-key "\C-xo" 'save-the-pinky-window)
;(global-set-key "\C-x\C-s" 'save-the-pinky-save)
;(global-set-key "\C-x\C-b" 'save-the-pinky-buffer)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key "\M-o" 'find-file)
;(global-set-key "\M-s" 'save-buffer)
(global-set-key [(meta down)] 'end-of-buffer)
(global-set-key [(meta up)] 'beginning-of-buffer)
(global-set-key [(meta right)] 'end-of-line)
(global-set-key [(meta left)] 'beginning-of-line)


(defun google (query)
  (interactive "sGoogle this: ")
  (browse-url (concat "http://google.co.uk/search?q=" query)))
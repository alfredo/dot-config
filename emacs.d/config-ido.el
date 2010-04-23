; ido
(require 'ido)
(ido-mode t)
(setq
 ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" 
    "^\\*tramp" "^\\*Messages\\*" " output\\*$")
  ido-everywhere t                 ; use for many file dialogs
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-file-list      50   ; remember many
  ido-enable-flex-matching t       ; be flexible
  ido-max-prospects 0              ; don't spam my minibuffer
)
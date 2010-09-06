;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(require 'find-cmd)
(require 'anything)
(require 'anything-config)
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))



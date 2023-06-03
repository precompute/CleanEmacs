(use-package undo-fu-session
  :config
  (setq undo-fu-session-directory
        (expand-file-name ".undo-fu-session/"
                          user-cache-directory))
  :init
  (undo-fu-session-global-mode))

;; -*- lexical-binding: t; -*-
(use-package undo-fu-session
  :config
  (setq undo-fu-session-directory
        (expand-file-name ".undo-fu-session/"
                          user-cache-directory)
        undo-fu-session-compression 'zst)
  :init
  (undo-fu-session-global-mode))

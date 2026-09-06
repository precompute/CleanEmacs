;; -*- lexical-binding: t; -*-
(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind ( :map completion-preview-active-mode-map
          ("<tab>" . completion-preview-complete)
          ("M-<tab>" . completion-preview-insert)
          ("M-n" . completion-preview-next-candidate)
          ("M-p" . completion-preview-prev-candidate))
  :config
  (setq completion-preview-minimum-symbol-length 2
        completion-preview-idle-delay 0.3
        completion-preview-message-format nil))

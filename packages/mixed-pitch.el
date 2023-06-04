(use-package mixed-pitch
  :hook ((org-mode
          info-mode
          markdown-mode) . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

(use-package mixed-pitch
  :hook ((org-mode
          info-mode) . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

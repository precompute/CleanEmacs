(use-package mixed-pitch
  :ensure ( :host github
            :repo "precompute/mixed-pitch")
  :hook ((org-mode
          info-mode
          markdown-mode) . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-all-attrs t))

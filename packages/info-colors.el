(use-package info-colors
  :ensure (:depth 1)
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

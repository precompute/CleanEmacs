(use-package info-colors
  :elpaca (:depth 1)
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package pulsar
  :hook ((window-selection-change-functions
          windmove-mode-hook
          imenu-after-jump-hook
          imenu-list-after-jump-hook
          consult-after-jump-hook
          evil-jumps-post-jump-hook) . pulsar-pulse-line)
  :custom
  (pulsar-face 'pulsar-green)
  :config
  (add-hook 'window-selection-change-functions-hook 'pulsar-pulse-line)
  :init
  (pulsar-global-mode))

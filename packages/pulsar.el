(use-package pulsar
  :hook ((windmove-mode-hook
          imenu-after-jump-hook
          imenu-list-after-jump-hook
          consult-after-jump-hook
          evil-jumps-post-jump-hook) . pulsar-pulse-line)
  :custom
  (pulsar-face 'pulsar-generic)
  (pulsar-iterations 1)
  (pulsar-delay 0)
  :config
  (defun pulsar-pulse-line--window-selection-c (_)
    "Wrapper around `pulsar-pulse-line’ to make it work with `window-selection-change-functions’."
    (pulsar-pulse-line))
  (add-hook 'window-selection-change-functions 'pulsar-pulse-line--window-selection-c)
  :init
  (pulsar-global-mode))

(use-package pulsar
  :hook ((windmove-mode-hook
          imenu-after-jump-hook
          imenu-list-after-jump-hook
          consult-after-jump-hook
          evil-jumps-post-jump-hook) . pulsar-pulse-line)
  :custom
  (pulsar-face 'pulsar-generic)
  (pulsar-iterations 1)
  (pulsar-delay 0.15)
  :config
  (defun pulsar-pulse-line--wrapper-c (_)
    "Wrapper around `pulsar-pulse-line’ for hooks."
    (pulsar-pulse-line))
  (dolist (f '(window-selection-change-functions))
    (add-hook f 'pulsar-pulse-line--wrapper-c))
  :init
  (pulsar-global-mode))

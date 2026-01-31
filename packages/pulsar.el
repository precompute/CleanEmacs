(use-package pulsar
  :hook ((windmove-mode
          imenu-after-jump
          imenu-list-after-jump
          consult-after-jump
          evil-jumps-post-jump) . pulsar-pulse-line)
  :custom
  (pulsar-face 'pulsar-generic)
  (pulsar-iterations 1)
  (pulsar-delay 0.3)
  :config
  (defun pulsar-pulse-line--wrapper-c (_)
    "Wrapper around `pulsar-pulse-line’ for hooks."
    (pulsar-pulse-line))
  (dolist (f '(window-selection-change-functions))
    (add-hook f 'pulsar-pulse-line--wrapper-c))
  :init
  (pulsar-global-mode))

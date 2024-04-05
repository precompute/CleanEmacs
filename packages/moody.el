(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function)
  (moody-replace-mode-line-buffer-identification))

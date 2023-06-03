(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function)
  (moody-replace-mode-line-buffer-identification)

  (setq-default header-line-format mode-line-format)
  (setq-default mode-line-format nil))

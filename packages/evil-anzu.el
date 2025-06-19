(use-package evil-anzu
  :after evil
  :init (global-anzu-mode)
  :config
  (setq-default anzu--mode-line-format nil))

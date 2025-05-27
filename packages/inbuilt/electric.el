(use-package electric
  :ensure nil
  :config
  (setq electric-quote-context-sensitive t
        electric-layout-allow-duplicate-newlines t
        electric-pair-open-newline-between-pairs t)
  :init
  (electric-indent-mode)
  (electric-quote-mode)
  (electric-layout-mode)
  (electric-pair-mode))

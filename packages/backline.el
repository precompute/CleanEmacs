(use-package backline
  :ensure (:depth 1)
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

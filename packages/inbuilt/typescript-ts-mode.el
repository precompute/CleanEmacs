(use-package typescript-ts-mode
  :ensure nil
  :hook ((typescript-ts-mode tsx-ts-mode) . (lambda () (electric-layout-local-mode -1))))

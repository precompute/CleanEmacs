(use-package eglot
  :elpaca nil
  :hook ((python-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp"))))

(use-package eglot
  :elpaca nil
  :hook ((python-ts-mode
          js-ts-mode
          lua-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(((python-mode python-ts-mode) . ("pylsp"))
                                        (lua-mode . "lua-language-server"))))

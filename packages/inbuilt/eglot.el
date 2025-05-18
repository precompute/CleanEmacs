(use-package eglot
  :elpaca nil
  :hook ((python-ts-mode
          js-mode
          js-ts-mode
          go-ts-mode
          zig-mode
          lua-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(zig-mode "zls"))
  ;; (add-to-list 'eglot-server-programs '(((python-mode python-ts-mode) . ("pylsp"))
  ;;                                       (lua-mode . "lua-language-server")
  ;;                                       ((go-mode go-ts-mode) . ("gopls"))))
  )

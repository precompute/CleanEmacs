(use-package eglot
  :ensure nil
  :hook ((python-mode
          python-ts-mode
          js-mode
          js-ts-mode
          go-mode
          go-ts-mode
          elixir-ts-mode
          zig-mode
          lua-mode) . eglot-ensure)
  :config
  (dolist (mode-server '((zig-mode . ("zls"))
                         (elixir-ts-mode . ("elixir-ls"))
                         ((go-mode go-ts-mode) . ("gopls"))
                         ((python-mode python-ts-mode) . ("ty" "server"))))
    (add-to-list 'eglot-server-programs mode-server)))

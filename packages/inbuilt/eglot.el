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
          rust-mode
          rust-ts-mode
          lua-mode) . eglot-ensure)
  :config
  (setq eglot-extend-to-xref t)
  (dolist (mode-server '((zig-mode . ("zls"))
                         (elixir-ts-mode . ("elixir-ls"))
                         ((go-mode go-ts-mode) . ("gopls"))
                         ((python-mode python-ts-mode) . ("ty" "server"))
                         ((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                                      (:check (:command "clippy"))))))
    (add-to-list 'eglot-server-programs mode-server)))

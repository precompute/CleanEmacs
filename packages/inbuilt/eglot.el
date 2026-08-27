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
  (setq eglot-extend-to-xref t
        eglot-autoshutdown t
        eglot-events-buffer-config '(:size 5000 :format short)
        eglot-documentation-renderer 'markdown-ts-view-mode)
  (dolist (mode-server '((zig-mode . ("zls"))
                         (elixir-ts-mode . ("elixir-ls"))
                         ((go-mode go-ts-mode) . ("gopls"))
                         ((python-mode python-ts-mode) . ("ty" "server"))
                         ((rust-mode rust-ts-mode) . ("rust-analyzer"))))
    (add-to-list 'eglot-server-programs mode-server)))

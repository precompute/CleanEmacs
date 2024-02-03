(use-package rustic
  :elpaca (:depth 1)
  :config
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer")
        rustic-lsp-client 'eglot))

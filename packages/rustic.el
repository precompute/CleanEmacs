(use-package rustic
  :ensure (:depth 1)
  :config
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer")
        rustic-lsp-client 'eglot))

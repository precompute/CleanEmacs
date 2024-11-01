(use-package llvm-mode
  :elpaca nil
  :config
  (add-to-list 'auto-mode-alist '("\\.ll$" . llvm-mode)))

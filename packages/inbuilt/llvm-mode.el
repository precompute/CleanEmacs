(use-package llvm-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.ll$" . llvm-mode)))

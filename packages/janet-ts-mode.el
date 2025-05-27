(use-package janet-ts-mode
  :ensure ( :host github
            :repo "sogaiu/janet-ts-mode"
            :files ("*.el" "extensions/*.el"))
  :config
  (add-to-list 'auto-mode-alist '("\\.janet$" . janet-ts-mode)))

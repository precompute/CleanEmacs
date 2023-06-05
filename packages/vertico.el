(use-package vertico
  :elpaca (:host github
                 :repo "minad/vertico"
                 :files ("*.el" "extensions/*.el"))
  :bind
  (:map vertico-map ("DEL" . vertico-directory-delete-char)) ;; delete entire folder names
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  (vertico-count-format
   (cons (propertize "%-10s" 'face 'font-lock-keyword-face)
         (concat
          (propertize "  %s" 'face 'font-lock-builtin-face)
          " %s")))
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy) ;; entering ~/ or //
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save) ;; save vertico for reinvocation
  :init
  (vertico-mode))

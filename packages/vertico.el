(use-package vertico
  :elpaca (:host github
           :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :bind
  (:map vertico-map ("DEL" . vertico-directory-delete-char))
  ;; :hook (minibuffer-setup-hook . vertico-repeat-save)
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  (vertico-count-format
   (cons (propertize "%-10s" 'face 'font-lock-keyword-face)
         (concat
          (propertize "  %s" 'face 'font-lock-builtin-face)
          " %s")))
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  :init
  (vertico-mode))

(use-package vertico
  :ensure ( :host github
            :repo "minad/vertico"
            :files ("*.el" "extensions/*.el"))
  :bind
  (:map vertico-map
        ("DEL" . vertico-directory-delete-char) ;; delete entire folder names
        ("C-SPC" . vertico-quick-insert)
        ("C-q" . vertico-quick-exit))
  :custom
  (vertico-count 25)
  (vertico-scroll-margin 5)
  (vertico-cycle t)
  ;; (vertico-count-format ;; slows things down
  ;;  (cons (propertize "%-10s" 'face 'font-lock-keyword-face)
  ;;        (concat
  ;;         (propertize "  %s" 'face 'font-lock-builtin-face)
  ;;         " %s")))
  (vertico-quick1 "mneio;")
  (vertico-quick2 "jlukh,")
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy) ;; entering ~/ or //
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save) ;; save vertico for reinvocation
  ;; (vertico-reverse-mode)
  :init
  (vertico-mode))

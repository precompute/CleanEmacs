(use-package ancilla
  :elpaca ( :host github
            :repo "shouya/ancilla.el"
            :files ("*.el"))
  :bind (("H-m" . ancilla-generate-or-rewrite)
         ("H-j" . ancilla-transient-menu)))

;; See private/other.el for API key

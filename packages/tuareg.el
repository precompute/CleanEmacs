(use-package tuareg
  :elpaca (:depth 1)
  :config
  (setq tuareg-indent-align-with-first-arg t
        tuareg-match-patterns-aligned t)
  (add-hook 'tuareg-mode-hook (lambda() (setq tuareg-mode-name "OCaml"))))

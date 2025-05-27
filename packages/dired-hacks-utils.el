(use-package dired-hacks-utils
  :ensure (:depth 1)
  :config
  (setq dired-subtree-cycle-depth 4
        dired-subtree-line-prefix ">")
  :bind (:map dired-mode-map
              ([tab] . dired-subtree-toggle)
              ([backtab] . dired-subtree-cycle)))

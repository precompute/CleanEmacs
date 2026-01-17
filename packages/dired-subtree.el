(use-package dired-subtree
  :ensure (:depth 1)
  :after dired
  :config
  (setq dired-subtree-cycle-depth 4
        dired-subtree-line-prefix ">")
  :bind (:map dired-mode-map
              ([tab] . dired-subtree-toggle)
              ([backtab] . dired-subtree-cycle)))

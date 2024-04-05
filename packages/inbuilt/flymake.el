(use-package flymake
  :elpaca nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-mode-line-format nil))

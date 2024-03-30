(use-package flymake
  :elpaca nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom (flymake-fringe-indicator-position 'right-fringe))

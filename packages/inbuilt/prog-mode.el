(use-package prog-mode
  :elpaca nil
  :hook ((prog-mode . hs-minor-mode)
         (prog-mode . display-fill-column-indicator-mode)))

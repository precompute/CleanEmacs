(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . hs-minor-mode)
         (prog-mode . display-fill-column-indicator-mode)))

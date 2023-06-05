(use-package dired
  :elpaca nil
  :hook (dired-mode-hook . evil-mode)
  ;; :hook (dired . evil)
  :hook (dired-mode-hook . display-line-numbers-mode)
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))

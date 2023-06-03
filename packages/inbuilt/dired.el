(use-package dired
  :elpaca nil
  :hook (dired-mode . evil-mode)
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))

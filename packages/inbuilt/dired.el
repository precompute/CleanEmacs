(use-package dired
  :elpaca nil
  :hook (dired . evil)
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))

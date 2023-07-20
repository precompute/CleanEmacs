(use-package dired
  :elpaca nil
  :hook (dired-mode . evil-mode)
  ;; :hook (dired . evil)
  :hook (dired-mode . display-line-numbers-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (dired-create-destination-dirs t)
  (dired-auto-revert-buffer t))

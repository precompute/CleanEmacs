(use-package dired
  :ensure nil
  :hook (dired-mode . evil-mode)
  :hook (dired-mode . display-line-numbers-mode)
  ;; :hook (dired-mode . diff-hl-dired-mode)
  :custom
  (dired-listing-switches "-ADFGhlNpv --group-directories-first --time-style=long-iso")
  ;; (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (dired-create-destination-dirs t)
  (dired-auto-revert-buffer t))

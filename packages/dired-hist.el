(use-package dired-hist
  :ensure (:depth 1)
  :hook (dired-mode . dired-hist-mode)
  :bind (:map dired-mode-map
              ("M-f" . dired-hist-go-forward)
              ("M-b" . dired-hist-go-back)))
;; [26-01-17 20:07:16] Disabling because dired-posframe messes up the history

(use-package dired-posframe
  :ensure (:depth 1)
  :bind (:map dired-mode-map
              ("M-p" . dired-posframe-mode))
  :config
  (setq dired-posframe-style 'point-bottom-left-upward
        dired-posframe-min-width nil
        dired-posframe-min-height nil
        dired-posframe-width nil
        dired-posframe-height nil
        dired-posframe-use-post-command-hook nil))

(use-package evil-goggles
  :config
  (setq evil-goggles-duration 0.15
        evil-goggles-pulse t)
  (dolist (c '((eval-region
                :face evil-goggles-paste-face
                :switch evil-goggles-enable-yank
                :advice evil-goggles--generic-async-advice)))
    (add-to-list 'evil-goggles--commands c))
  :init
  (evil-goggles-mode))

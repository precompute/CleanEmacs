(use-package which-key
  :defer t
  :config
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.05
        which-key-show-prefix 'left
        which-key-popup-type 'minibuffer
        which-key-min-display-lines 4
        which-key-allow-imprecise-window-fit t)
  :init
  (which-key-mode))

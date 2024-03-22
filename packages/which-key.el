(use-package which-key
  :defer t
  :config
  (setq which-key-idle-delay 1
        which-key-idle-secondary-delay 1
        which-key-show-prefix 'left
        which-key-popup-type 'minibuffer
        which-key-min-display-lines 7
        which-key-allow-imprecise-window-fit t)
  :init
  (which-key-mode))

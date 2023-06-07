(use-package which-key
  :defer t
  :config
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'minibuffer)
  :init
  (which-key-mode))

(use-package evil-snipe
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :init
  (evil-snipe-mode)
  (evil-snipe-override-mode))

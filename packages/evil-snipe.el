(use-package evil-snipe
  :preface
  (setq evil-snipe-use-vim-sneak-bindings t)
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t
        evil-snipe-tab-increment t)
  :init
  (evil-snipe-mode)
  (evil-snipe-override-mode))

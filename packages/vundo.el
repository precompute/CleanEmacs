(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-window-side 'top
        vundo-compact-display t
        vundo-window-max-height 7
        vundo-glyph-alist vundo-unicode-symbols))

(use-package vundo
  :bind ("C-x u" . vundo)
  :defer t
  :config
  (setq vundo-window-side 'top
        vundo-compact-display t
        vundo-window-max-height 7
        vundo-glyph-alist '((selected-node . 9723) ;; 9679
                            (node . 9724) ;; 9675
                            (horizontal-stem . 9472)
                            (vertical-stem . 9474)
                            (branch . 9500)
                            (last-branch . 9492))))

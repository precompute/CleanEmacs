(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name ".undo-tree/"
                                    user-cache-directory))))
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)
  :init
  (global-undo-tree-mode))

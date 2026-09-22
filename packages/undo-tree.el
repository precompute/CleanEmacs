;; -*- lexical-binding: t; -*-
(use-package undo-tree
  :bind ("C-x u" . undo-tree-visualize)
  :defer t
  :config
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name ".undo-tree/"
                                    user-cache-directory))))
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t)
  :init
  (global-undo-tree-mode))

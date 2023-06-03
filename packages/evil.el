;; packages/evil.el -*- lexical-binding: t; -*-
(use-package evil
  :config
  (setq evil-undo-system 'undo-fu
        evil-split-window-below t
        evil-vsplit-window-right t)

  (evil-define-text-object evil-textobj-whole-buffer (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))

  :init
  (setq evil-want-keybinding nil)
  (evil-mode))


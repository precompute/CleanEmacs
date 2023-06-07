;; packages/evil.el -*- lexical-binding: t; -*-
(use-package evil
  :config
  (setq evil-undo-system 'undo-redo ;; 'undo-fu
        evil-undo-function #'undo
        evil-redo-function #'undo-redo
        ;; evil-undo-function #'undo-tree-undo
        ;; evil-redo-function #'undo-tree-redo
        ;; evil-undo-function #'undo-fu-only-undo
        ;; evil-redo-function #'undo-fu-only-redo
        evil-split-window-below t
        evil-vsplit-window-right t)
  (setq evil-symbol-word-search t
        evil-shift-width 2)
  (setq evil-want-Y-yank-to-eol t)

  (evil-define-text-object evil-textobj-whole-buffer (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))

  (evil-define-text-object evil-textobj-get-func (count &optional _beg _end type)
    "Text object to select the top-level Lisp form or function definition at
point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end type)))

  :init
  (setq evil-want-keybinding nil)
  (evil-mode))


;; packages/evil.el -*- lexical-binding: t; -*-
(use-package evil
  :config
  (if (boundp 'undo-tree-mode)
      (setq evil-undo-system 'undo-tree
            evil-undo-function #'undo-tree-undo
            evil-redo-function #'undo-tree-redo)
    (if (boundp 'undo-fu-mode)
        (setq evil-undo-system 'undo-fu
              evil-undo-function #'undo-fu-only-undo
              evil-redo-function #'undo-fu-only-redo)
      (setq evil-undo-system 'undo-redo ;; 'undo-fu
            evil-undo-function #'undo
            evil-redo-function #'undo-redo)))
  (setq evil-split-window-below t
        evil-vsplit-window-right t)
  (setq evil-symbol-word-search t
        evil-shift-width 2)
  (setq evil-want-Y-yank-to-eol t ;; Doesn’t work
        evil-want-C-g-bindings t
        evil-want-C-i-jump t
        evil-respect-visual-line-mode t)
  (setq evil-insert-state-cursor '(hbar . 1)
        evil-normal-state-cursor '(hbar . 4)
        evil-visual-state-cursor 'hbar
        evil-replace-state-cursor 'hollow
        evil-motion-state-cursor 'hollow
        evil-operator-state-cursor 'hollow
        evil-emacs-state-cursor 'bar)
  (evil-put-command-property 'evil-yank-line :motion 'evil-end-of-line-or-visual-line) ;; workaround for evil-want-Y-yank-to-eol

  (evil-define-text-object evil-textobj-whole-buffer (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))

  (evil-define-text-object evil-textobj-get-func (count &optional _beg _end type)
    "Text object to select the top-level Lisp form or function definition at
point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end type)))

  (evil-define-text-object evil-textobj-entire-line (count &optional _beg _end type)
    "Text object for the current line"
    (evil-range (pos-bol) (pos-eol) type))

  (evil-define-text-object evil-textobj-forward-until-empty-line (count &optional _beg _end type)
    "Text object until the end of the current paragraph"
    (evil-range (point) (re-search-forward "\\(?:$\\)\\(?:^\\)[[:space:]]*$") type))

  (evil-define-text-object evil-textobj-backward-until-empty-line (count &optional _beg _end type)
    "Text object until the start of the current paragraph"
    (evil-range (point) (re-search-backward "\\(?:$\\)\\(?:^\\)[[:space:]]*$") type))

  :init
  (setq evil-want-keybinding nil)
  (evil-mode))


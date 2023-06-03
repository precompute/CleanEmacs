;; packages/evil.el -*- lexical-binding: t; -*-
(use-package evil
  :config
  (setq evil-undo-system 'undo-fu
        evil-split-window-below t
        evil-vsplit-window-right t)
;;  (defun evil-window-split-follow ()
;;    (interactive)
;;    (let ((evil-split-window-below (not evil-split-window-below)))
;;      (call-interactively #'evil-window-split)))
;;  (defun evil-window-vsplit-follow ()
;;    (interactive)
;;    (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
;;      (call-interactively #'evil-window-vsplit)))
;;  (global-set-key [remap evil-window-split] #'evil-window-split-follow)
;;  (global-set-key [remap evil-window-vsplit] #'evil-window-vsplit-follow)

  :init
  (setq evil-want-keybinding nil)
  (evil-mode))


;; -*- lexical-binding: t; -*-
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0.2
        show-paren-style 'expression
        show-paren-not-in-comments-or-strings t))

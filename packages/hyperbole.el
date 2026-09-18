;; -*- lexical-binding: t; -*-
(use-package hyperbole
  :ensure (:files ("*" "man/*" (:exclude "man")))
  :init (require 'dired)
  :defer t
  :config
  (hyperbole-mode 1))

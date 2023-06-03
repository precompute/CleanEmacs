;; general.el -*- lexical-binding: t; -*-

(setq use-short-answers t) ;; yes or no -> y or n 
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward)
(setq mouse-yank-at-point t)

(setq ring-bell-function #'ignore)
(setq visible-bell nil)

(setq x-stretch-cursor t)
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

(winner-mode)

;; -*- lexical-binding: t; -*-
(use-package tramp
  :ensure nil
  :config
  (connection-local-set-profile-variables
   'remote-c
   '((dired-listing-switches . "-al")))
  (connection-local-set-profiles
   '(:application tramp)
   'remote-c))

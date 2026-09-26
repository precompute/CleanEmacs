;; -*- lexical-binding: t; -*-
(use-package isearch
  :ensure nil
  :custom
  (lazy-highlight-cleanup nil)
  (lazy-highlight-initial-delay 0.1)
  :config
  ;; Make search strings buffer-local
  (dolist (z '(search-ring regexp-search-ring))
    (make-variable-buffer-local z)))

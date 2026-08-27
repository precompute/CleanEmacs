(use-package isearch
  :ensure nil
  :config
  ;; Make search strings buffer-local
  (dolist (z '(search-ring regexp-search-ring))
    (make-variable-buffer-local z)))

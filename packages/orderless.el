(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp))
  (completion-styles '(orderless basic partial-completion initials shorthand))
  ;; (completion-styles '(orderless basic partial-completion emacs22 emacs21 flex initials shorthand))
  ;; (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-category-overrides nil)
  (orderless-expand-substring 'substring))

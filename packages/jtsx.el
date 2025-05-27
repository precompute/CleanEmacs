(use-package jtsx
  :ensure (:depth 1)
  :config
  (add-to-list 'auto-mode-alist (cons "\\.tsx\\'" 'jtsx-tsx-mode))
  (add-to-list 'auto-mode-alist (cons "\\.jsx\\'" 'jtsx-jsx-mode)))

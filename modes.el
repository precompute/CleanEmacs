;;;; -*- lexical-binding: t; -*-

;;;; modes
(dolist (z (list (cons "\\.jsx\\'" 'js-jsx-mode)))
  (add-to-list 'auto-mode-alist z))

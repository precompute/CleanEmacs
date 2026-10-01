;; -*- lexical-binding: t; -*-
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons nil)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

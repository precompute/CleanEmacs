;; -*- lexical-binding: t; -*-
(use-package savehist
  :ensure nil
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  :init
  (savehist-mode))

;; -*- lexical-binding: t; -*-
(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :config
  (setq markdown-ts--set-up-inline t))

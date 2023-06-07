(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\:)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-delay 0.2)
  :hook (prog-mode . highlight-indent-guides-mode))

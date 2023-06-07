(use-package diff-hl
  ;; :hook (prog-mode . diff-hl)
  :defer t
  :config
  (setq vc-git-diff-switches '("--histogram"))
  :init
  (global-diff-hl-mode))

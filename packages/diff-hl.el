(use-package diff-hl
  ;; :hook (prog-mode . diff-hl)
  :config
  (setq vc-git-diff-switches '("--histogram"))
  :init
  (global-diff-hl-mode))

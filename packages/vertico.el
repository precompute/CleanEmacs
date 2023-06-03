(use-package vertico
  :custom
  (vertico-count 20)
  (setq vertico-count-format
        (cons (propertize "%-10s" 'face 'font-lock-keyword-face)
              (concat
               (propertize "  %s" 'face 'font-lock-builtin-face)
               " %s")))
  :init
  (vertico-mode))

(use-package corfu-candidate-overlay
  :elpaca (:depth 1)
  :after corfu
  :bind ("C-<return>" . corfu-candidate-overlay-complete-at-point))

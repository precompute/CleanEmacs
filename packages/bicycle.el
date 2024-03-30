(use-package bicycle
  :after prog-mode
  :bind (:map prog-mode-map
              ([C-tab] . bicycle-cycle)
              ([backtab] . bicycle-cycle-global)))

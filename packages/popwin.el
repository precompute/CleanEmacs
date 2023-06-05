(use-package popwin
  :config
  (push '(helpful-mode :position bottom :height 0.2) popwin:special-display-config)
  (push '(magit-status-mode :position right :width 0.5) popwin:special-display-config)
  (push '(ilist-mode :position left :width 0.2) popwin:special-display-config)
  (push '(dired-sidebar-mode :position left :width 0.2) popwin:special-display-config)
  :init
  (popwin-mode))

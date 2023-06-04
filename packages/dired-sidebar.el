(use-package dired-sidebar
  :hook (toggle-truncate-lines)
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-display-alist '((side . left) (slot . 0)))
  (add-hook 'dired-sidebar-mode-hook #'(lambda ()
                                         (setq-local truncate-lines t))
            -90)
  (setq dired-sidebar-width 25
        dired-sidebar-theme 'ascii
        dired-sidebar-tui-update-delay 0
        dired-sidebar-recenter-cursor-on-tui-update t
        dired-sidebar-no-delete-other-windows t
        dired-sidebar-use-custom-modeline t))

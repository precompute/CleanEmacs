(use-package imenu-list
  :hook (imenu-mode . imenu-list-mode)
  :config
  (setq imenu-list-position 'left
        imenu-list-size 40
        imenu-list-idle-update-delay 1
        imenu-list-focus-after-activation t))

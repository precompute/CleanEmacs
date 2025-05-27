(use-package mlscroll
  :ensure (:depth 1)
  :defer t
  :custom
  (mlscroll-right-align nil)
  :init
  (mlscroll-mode)
  :config
  (setq mlscroll-extra-properties
        `(local-map
          (keymap (header-line keymap
                               (down-mouse-1 . mlscroll-mouse)
                               (wheel-left . ignore)
                               (wheel-right . ignore)
                               (,mouse-wheel-up-event . mlscroll-wheel)
                               (,mouse-wheel-down-event . mlscroll-wheel)))
          help-echo "mouse-1: scroll buffer"
          mlscroll t)
        mlscroll-width-chars 10))

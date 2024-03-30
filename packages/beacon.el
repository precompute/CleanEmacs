(use-package beacon
  :elpaca (:depth 1)
  :config
  (setq beacon-blink-when-focused t
        beacon-size 10)
  (defun set-beacon-color ()
    (interactive)
    (setq beacon-color (face-attribute 'font-lock-builtin-face :foreground)))
  (add-to-list 'enable-theme-functions 'set-beacon-color)
  (beacon-mode))

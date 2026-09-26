(use-package help-at-pt
  :ensure nil
  :config
  (setq-default help-at-pt-display-when-idle t
                help-at-pt-timer-delay 0.5)
  (help-at-pt-set-timer)) ;; hints in the echo area

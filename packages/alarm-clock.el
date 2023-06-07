(use-package alarm-clock
  :elpaca (:depth 1)
  :defer t
  :config
  (setq alarm-clock-cache-file (expand-file-name ".alarm-clock.cache" user-cache-directory)))

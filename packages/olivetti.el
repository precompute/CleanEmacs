(use-package olivetti
  :hook ((prog-mode
	  text-mode
          dired-mode
          magit-mode
          info-mode
          conf-colon-mode
          helpful-mode) . olivetti-mode)
  :config
  (setq-default olivetti-body-width 130
                olivetti-margin-width 2)
  (add-hook 'org-mode-hook #'(lambda () (interactive) (setq-local olivetti-body-width 80)))
  ;; Remove all olivetti keybinds.  Interferes with org-mode bindings.
  (assq-delete-all 'olivetti-mode minor-mode-map-alist))

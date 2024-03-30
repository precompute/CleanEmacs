(use-package perfect-margin
  :elpaca (:depth 1)
  :hook ((prog-mode
          text-mode
          dired-mode
          magit-mode) . perfect-margin-mode)
  :config
  (setq-default perfect-margin-lighter nil
                perfect-margin-visible-width 130)
  (add-hook 'org-mode-hook #'(lambda () (interactive) (setq-local perfect-margin-visible-width 80))))

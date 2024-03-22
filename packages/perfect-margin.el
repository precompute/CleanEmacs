(use-package perfect-margin
  :elpaca (:depth 1)
  :hook ((prog-mode
          text-mode
          dired-mode
          vertico-mode
          vertico-reverse-mode
          magit-mode
          helpful-mode
          help-mode
          ibuffer-mode
          Info-mode
          woman-mode
          minibuffer-mode) . perfect-margin-mode)
  :config
  (setq-default perfect-margin-lighter nil
                perfect-margin-visible-width 130)
  (add-hook 'org-mode-hook #'(lambda () (interactive) (setq-local perfect-margin-visible-width 80))))

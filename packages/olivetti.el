(use-package olivetti
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
	  minibuffer-mode) . olivetti-mode)
  :config
  (setq-default olivetti-body-width 130
                olivetti-margin-width 2
                olivetti-recall-visual-line-mode-entry-state t)
  (add-hook 'org-mode-hook #'(lambda () (interactive) (setq-local olivetti-body-width 80))))

(use-package olivetti
  :hook ((prog-mode
	  text-mode
          dired-mode
          vertico-mode
          magit-mode
          helpful-mode
          help-mode
          ibuffer-mode
          Info-mode
	  minibuffer-mode) . olivetti-mode)
  :config
  (setq-default olivetti-body-width 130
                olivetti-margin-width 0
                olivetti-recall-visual-line-mode-entry-state t
                olivetti-style 'fancy))

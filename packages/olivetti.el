(use-package olivetti
  :hook ((prog-mode
	  text-mode
	  minibuffer-mode-hook) . olivetti-mode)
  :config
  (setq olivetti-body-width 150
        olivetti-magin-width 0
	olivetti-recall-visual-line-mode-entry-state t
	olivetti-style 'fancy))

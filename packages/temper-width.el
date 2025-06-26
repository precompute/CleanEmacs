(use-package temper-width
  :ensure ( :depth 1
            :repo "~/44.2/temper-width")
  :config
  (setq-default temper-width-width 120)
  (setq temper-width-enable-on-prog-derived t
        temper-width-allowed-modes '( text-mode dired-mode
                                      magit-mode Info-mode
                                      conf-colon-mode helpful-mode
                                      shortdoc-mode org-mode
                                      lexic-mode)))

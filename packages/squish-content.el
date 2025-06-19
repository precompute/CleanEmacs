(use-package squish-content
  :ensure ( :depth 1
            :repo "~/44.2/squish-content/")
  :config
  (squish-content-mode t)
  (setq squish-content-advice-around-functions
        (append squish-content-advice-around-functions
                '( evil-window-split
                   evil-window-vsplit)))
  (setq squish-content-advice-after-functions
        (append squish-content-advice-after-functions
                '(dired-jump))))

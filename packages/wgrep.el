(use-package wgrep
  :ensure (:depth 1)
  :bind (:map wgrep-mode-map
              ("ZS" . (lambda () (interactive)
                        (progn (wgrep-finish-edit) (wgrep-save-all-buffers))))))

(use-package js
  :ensure nil
  :hook (js-mode . (lambda ()
                     (setq-local electric-layout-rules
                                 (assq-delete-all 59 electric-layout-rules)))))

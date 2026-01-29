(use-package js
  :ensure nil
  :hook (js-mode . js-mode-setup-c)
  :custom (js-indent-level 2)
  :config
  (defun js-mode-setup-c ()
    "Setup for js-mode."
    (setq-local electric-layout-rules (assq-delete-all 59 electric-layout-rules))))

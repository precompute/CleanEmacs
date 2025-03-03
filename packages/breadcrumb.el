(use-package breadcrumb
  :elpaca (:depth 1)
  :config
  (setq breadcrumb-imenu-max-length 1.0
        breadcrumb-imenu-crumb-separator "::")
  (defvar enable-breadcrumb? nil
    "Dummy var to recognize when Breadcrumbs need to be enabled.")
  (defun set-var-for-breadcrumb ()
    "Set `enable-breadcrumb?’ in the current buffer."
    (setq-local enable-breadcrumb? t))
  ;; slow? see `imenu-max-index-time’
  (add-hook 'prog-mode-hook #'set-var-for-breadcrumb))

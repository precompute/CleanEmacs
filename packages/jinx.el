(use-package jinx
  :ensure (:depth 1)
  :config
  (defun jinx-next-evil-wrapper (&rest _args)
    (when (and evil-mode (not (bobp))) (backward-char 1)))
  (advice-add 'jinx-next :after #'jinx-next-evil-wrapper))

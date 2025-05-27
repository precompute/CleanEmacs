(use-package highlight-defined
  :ensure (:depth 1)
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :config
  (setq highlight-defined-face-use-itself t)
  (set-face-attribute 'highlight-defined-builtin-function-name-face nil
                      :inherit 'font-lock-keyword-face)
  (set-face-attribute 'highlight-defined-function-name-face nil
                      :inherit 'font-lock-keyword-face)
  (set-face-attribute 'highlight-defined-face-name-face nil
                      :inherit 'font-lock-number-face)
  (set-face-attribute 'highlight-defined-macro-name-face nil
                      :weight 'bold
                      :inherit 'font-lock-number-face))

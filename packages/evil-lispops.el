(use-package evil-lispops
  :ensure (:depth 1)
  :hook ((emacs-lisp-mode-hook
          clojure-mode-hook) . evil-lispops-mode))

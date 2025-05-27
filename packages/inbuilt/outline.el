(use-package outline
  :ensure nil
  :hook ((clojure-mode
          clojure-ts-mode
          emacs-lisp-mode
          lua-mode
          lua-ts-mode
          ;; python-mode
          ;; python-ts-mode
          rust-mode
          rust-ts-mode) . outline-minor-mode)
  :preface (setq outline-minor-mode-map nil))

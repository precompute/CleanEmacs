(use-package outline
  :ensure nil
  :hook ((clojure-mode
          clojure-ts-mode
          emacs-lisp-mode
          sh-mode
          lua-mode
          lua-ts-mode
          ;; python-mode
          ;; python-ts-mode
          js-mode
          js-base-mode
          rust-mode
          rust-ts-mode) . outline-minor-mode)
  ((js-mode js-base-mode) . (lambda () (setq-local outline-regexp js-mode-outline-regexp-c
                                                   outline-level #'outline-level-group-1-c)))
  (sh-mode . (lambda () (setq-local outline-regexp sh-mode-outline-regexp-c
                                    outline-level #'outline-level-group-1-c)))
  :preface (setq outline-minor-mode-map nil)
  :config
  (setq js-mode-outline-regexp-c (rx (* (or space blank)) (= 2 ?/) (or space blank) (group (+ "*"))))
  (setq sh-mode-outline-regexp-c (rx bol "###" (or space blank) (group (+ "*"))))
  (defun outline-level-group-1-c ()
    "Calculate the characters in the first group of the match."
    (- (match-end 1) (match-beginning 1))))

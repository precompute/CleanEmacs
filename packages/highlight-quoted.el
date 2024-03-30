(use-package highlight-quoted
  :hook ((emacs-lisp-mode
	  clojure-mode) . highlight-quoted-mode))

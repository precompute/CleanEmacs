(use-package outshine
  :elpaca (:depth 1)
  :hook (emacs-lisp-mode . outshine-mode)
  :config
  ;; Remove all keybinds.
  (assq-delete-all 'outshine-mode minor-mode-map-alist))

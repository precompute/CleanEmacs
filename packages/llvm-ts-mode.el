(use-package llvm-ts-mode
  :ensure (:depth 1)
  ;; :hook (llvm-ts-mode . turn-off-hideshow)
  :config
  ;; (defun enable-llvm-ts-mode-c ()
  ;;   "Enable `llvm-ts-mode’ after `turn-off-hideshow’."
  ;;   (turn-off-hideshow)
  ;;   (llvm-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ll$" . enable-llvm-ts-mode-c)))

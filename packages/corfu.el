;; -*- lexical-binding: t; -*-
(use-package corfu
  :ensure (:host github
                 :repo "minad/corfu"
                 :files ("*.el" "extensions/*.el"))
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :config
  (setq corfu-auto nil
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary 'separator
        corfu-preview-current nil
        corfu-scroll-margin 2)
  (setq corfu-popupinfo-delay (cons 0.25 0.25)
        corfu-popupinfo-max-height 30
        corfu-popupinfo-hide nil)
  (setq corfu-separator ?\s)
  ;; (with-eval-after-load 'evil
  ;;   (setq evil-complete-next-func (lambda (_) (completion-at-point))))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  :hook (prog-mode . corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode))

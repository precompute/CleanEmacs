;; -*- lexical-binding: t; -*-
(use-package dired
  :ensure nil
  :hook ((dired-mode . evil-mode)
         (dired-mode . display-line-numbers-mode)
         ;; (dired-before-readin . dired-set-listing-switches-c)
         ;; (dired-mode . dired-set-listing-switches-c)
         )
  :custom
  (dired-dwim-target t)
  (dired-create-destination-dirs t)
  (dired-auto-revert-buffer t)
  (dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura")))
  (dired-auto-toggle-b-switch t)
  (dired-listing-switches "-AFGhlNpv --group-directories-first --time-style=long-iso")
  ;; :config
  ;; (defun dired-set-listing-switches-c ()
  ;;   (interactive)
  ;;   ;; with-current-buffer (current-buffer)
  ;;     (unless (file-remote-p default-directory)
  ;;       (setq-local dired-listing-switches
  ;;                   "-AFGhlNpv --group-directories-first --time-style=long-iso")))
  ;; ;; (advice-add 'dired-readin :before (lambda (&rest _) (dired-set-listing-switches-c)))
  )

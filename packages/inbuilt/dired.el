;; -*- lexical-binding: t; -*-
(use-package dired
  :ensure nil
  :hook ((dired-mode . evil-mode)
         (dired-mode . display-line-numbers-mode)
         (dired-mode . dired-fontify-filename-c))
  :custom
  (dired-dwim-target t)
  (dired-create-destination-dirs t)
  (dired-auto-revert-buffer t)
  (dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura")))
  (dired-listing-switches "-AFGhlNpv --group-directories-first --time-style=long-iso")
  :config
  (defface dired-filename-face-c '((t (:inherit variable-pitch)))
    "Face for dired filenames.")
  (defface dired-other-face-c '((t (:inherit shadow)))
    "Face for text before dired filenames.")
  (defface dired-date-face-c '((t (:inherit (region bold))))
    "Face for text before dired filenames.")
  (defface dired-filename-decorator-prefix-face-c '((t (:inherit (font-lock-keyword-face fixed-pitch-numbers))))
    "Face for decorator before dired filenames.")
  (defface dired-filename-extension-face-c '((t (:inherit font-lock-builtin-face)))
    "Face for dired filename extensions.")
  (defun dired-fontify-filename-c ()
    (font-lock-add-keywords nil
     '(("^\\(.*\\)\\( \\)\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\)\\( \\)\\(.*\\)$"
        (1 'dired-other-face-c prepend)
        ;; (2 '(face dired-filename-decorator-prefix-face-c display " • ") prepend)
        (3 'dired-date-face-c prepend)
        ;; (4 '(face dired-filename-decorator-prefix-face-c display " ⌇ ") prepend)
        (5 'dired-filename-face-c prepend))
       ("\\.[a-zA-Z0-9]+$"
        (0 'dired-filename-extension-face-c prepend)))
     'append)))

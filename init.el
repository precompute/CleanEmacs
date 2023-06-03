;; init.el -*- lexical-binding: t; -*-
(defmacro loadfile-c (file)
  "load file from `user-emacs-directory'"
  `(load-file (expand-file-name ,file user-emacs-directory)))
(loadfile-c "elpaca-init.el")
(loadfile-c "general.el")
(loadfile-c "ui.el")
;; keybinds.el loaded after General-mode

(defun after-init-load-file ()
  (progn
    (loadfile-c "functions.el")
    (loadfile-c "keybinds.el")))
(add-hook 'elpaca-after-init-hook 'after-init-load-file)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

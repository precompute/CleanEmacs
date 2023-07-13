;; init.el -*- lexical-binding: t; -*-
(defmacro loadfile-c (file)
  "load file from `user-emacs-directory'"
  `(load-file (expand-file-name ,file user-emacs-directory)))
(loadfile-c "elpaca-init.el")
(loadfile-c "general.el")
;; keybinds.el loaded after General-mode

(defun after-init-load-file ()
  (progn
    (loadfile-c "functions.el")
    (loadfile-c "keybinds.el")
    (loadfile-c "hooks.el")
    (loadfile-c "ui.el")
    (loadfile-c "modeline.el")
    (loadfile-c "modes.el")
    (loadfile-c "loads.el")))

(add-hook 'elpaca-after-init-hook 'after-init-load-file)

(if (not (file-exists-p "/run/user/1000/emacs/server"))
    (server-start)
  (message "Emacs Server already running, run (server-start) after setting `server-name’ otherwise."))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

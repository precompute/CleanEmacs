;; init.el -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 4 1024 1024))

(defun loadfile-c (file)
  "Load FILE from `user-emacs-directory'."
  (let ((f (expand-file-name file user-emacs-directory)))
    (when (file-exists-p f)
      (load-file f))))
(loadfile-c "elpaca-init.el")
(loadfile-c "general.el")
;; keybinds.el loaded after General-mode

(defun native-compile-user-programs ()
  "Native Compile selected user programs."
  (interactive)
  (dolist (z (list "headerline-simple.el" "functions.el" "ui.el"))
    (native-compile (expand-file-name z user-emacs-directory)))
  (native-compile-directory (expand-file-name "packages/" user-emacs-directory)))
(native-compile-user-programs)

(defun after-init-load-file ()
  (progn
    (loadfile-c "functions.el")
    (loadfile-c "keybinds.el")
    ;; (loadfile-c "hooks.el")
    (loadfile-c "ui.el")
    (loadfile-c "headerline-simple.el")
    (loadfile-c "modes.el")
    (loadfile-c "loads.el")
    (loadfile-c ".private/other.el")))

(add-hook 'elpaca-after-init-hook 'after-init-load-file)

(if (not (file-exists-p "/run/user/1000/emacs/server"))
    (server-start)
  (message "Emacs Server already running, run (server-start) after setting `server-name’ otherwise."))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

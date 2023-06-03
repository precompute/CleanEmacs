;; init.el -*- lexical-binding: t; -*-
;;; Elpaca
;;;; Init
(load-file (expand-file-name "elpaca-init.el" user-emacs-directory))
(load-file (expand-file-name "ui.el" user-emacs-directory))
(load-file (expand-file-name "general.el" user-emacs-directory))
;; (load-file (expand-file-name "keybinds.el" user-emacs-directory)) ;; loaded after General

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

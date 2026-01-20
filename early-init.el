(setq gc-cons-threshold (* 8 gc-cons-threshold))
(setq package-enable-at-startup nil)

(setq native-comp-always-compile t
      native-comp-async-jobs-number 12)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      inhibit-x-resources t
      create-lockfiles nil
      make-backup-files nil)

(setq byte-compile-docstring-max-column 200)

(setq warning-minimum-level :error) ;; for pesky documentation errors

(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
(set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)

(load-file (expand-file-name "early-init-ui.el" user-emacs-directory))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

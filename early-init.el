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

;; Prevent white flash.  Use `default-frame-alist’ not `set-face-attribute’, so that these
;; properties can be removed from `default-frame-alist’ in init.el.
;; Using `set-face-attribute’ messes up face loading in new frames and pollutes the default setup.
(add-to-list 'default-frame-alist '(background-color . "#000000"))
(add-to-list 'default-frame-alist '(foreground-color . "#ffffff"))

(load-file (expand-file-name "early-init-ui.el" user-emacs-directory))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

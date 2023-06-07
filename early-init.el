(setq package-enable-at-startup nil)
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      create-lockfiles nil
      make-backup-files nil)

(setq byte-compile-docstring-max-column 9000)

;; (setq warning-minimum-level :error)

(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
(set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Makes startup faster."
            (let ((tempgcval gc-cons-threshold)
                  (gc-cons-threshold most-positive-fixnum))
              (setq gc-cons-threshold tempgcval))))

(load-file (expand-file-name "early-init-ui.el" user-emacs-directory))

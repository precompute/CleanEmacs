(setq package-enable-at-startup nil)
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      create-lockfiles nil
      make-backup-files nil)

(setq warning-minimum-level :error)

(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
(set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)

(load-file (expand-file-name "early-init-ui.el" user-emacs-directory))

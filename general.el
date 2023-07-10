;; general.el -*- lexical-binding: t; -*-

(setq use-short-answers t) ;; yes or no -> y or n 
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward)
(setq mouse-yank-at-point t)

(setq ring-bell-function #'ignore)
(setq visible-bell nil)
(setq initial-buffer-choice t) ;; start with scratch buffer
(setq initial-scratch-message (concat
                               ";; Scratch Buffer.\n"
                               ";; Startup Time: " (emacs-init-time) "\n"
                               ";; Time: " (current-time-string)))
(setq coding-system-for-write 'raw-text)

(setq frame-title-format "Emacs - %f")

(setq x-stretch-cursor t)
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)
(setq scroll-margin 10)
(setq scroll-conservatively 101)
(setq window-divider-default-right-width 10
      window-divider-default-bottom-width 0)
(setq-default left-fringe-width 5) ;; Will it work?

(setq history-length 999)

(setq recentf-max-menu-items 999
      recentf-max-saved-items 999)

(setq load-prefer-newer t)

(setq enable-recursive-minibuffers t)

(setq vc-follow-symlinks t) ;; no prompting for changing VC'd files

(setq-default indent-tabs-mode nil)

(setq display-line-numbers-width 5)

(setq revert-without-query '(".")) ;; no query for revert

(setq confirm-kill-emacs 'y-or-n-p)

(setq blink-cursor-delay 0.2
      blink-cursor-interval 0.1
      blink-cursor-blinks 0)

(defmacro setcache-c (var file)
  "Macro to set file location to `user-cache-directory'"
  `(setq-default ,var (expand-file-name ,file user-cache-directory)))
(setcache-c recentf-save-file      "recentf")
(setcache-c project-list-file      "projects")
(setcache-c savehist-file          "history")
(setcache-c transient-history-file "transient/history")
(setcache-c transient-levels-file  "transient/levels")
(setcache-c transient-values-file  "transient/values")
(setcache-c bookmark-default-file  "bookmarks")
(setcache-c save-place-file        "places")

(winner-mode 1)
(fringe-mode '(3 . 0))
(savehist-mode 1)
(save-place-mode 1)
;; (global-display-line-numbers-mode) ;; Don’t need it.

(setq-default help-at-pt-display-when-idle t
              help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer) ;; hints in the echo area

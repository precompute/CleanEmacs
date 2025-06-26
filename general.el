;; general.el -*- lexical-binding: t; -*-

(setq use-short-answers t) ;; yes or no -> y or n 
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward)
(setq mouse-yank-at-point t)

(setq ring-bell-function #'ignore)
(setq visible-bell t)
(setq initial-buffer-choice t) ;; start with scratch buffer
(setq initial-scratch-message (concat
                               ";; Scratch Buffer.\n"
                               ";; Startup Time: " (emacs-init-time) "\n"
                               ";; Time: " (current-time-string)))
(setq coding-system-for-write 'raw-text)

(setq-default indicate-empty-lines t)

(setq frame-title-format "Emacs - %f")

(setq x-stretch-cursor t)
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)
(setq scroll-margin 10)
(setq scroll-conservatively 101)

(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))

(setq history-length 150)

(setq recentf-max-menu-items 150
      recentf-max-saved-items 150)

(setq load-prefer-newer t)

(setq enable-recursive-minibuffers t)

(setq vc-follow-symlinks t) ;; no prompting for changing VC'd files

(setq-default indent-tabs-mode nil)

(setq display-line-numbers-width 5)

(setq revert-without-query '(".")) ;; no query for revert

(setq confirm-kill-emacs 'y-or-n-p)

(setq blink-cursor-delay 0.25
      blink-cursor-interval 0.25
      blink-cursor-blinks 0)
(blink-cursor-mode 1)

(setq-default fill-column 97)

(setq disabled-command-function nil)

(setq lazy-highlight-cleanup nil
      lazy-highlight-initial-delay 0.1)

(setq messages-buffer-max-lines 100000)

(setq undo-limit 240000)

(setq-default indicate-buffer-boundaries '((top . left) (bottom . left) (up . right) (down . right)))

(minibuffer-depth-indicate-mode t)

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

(setq desktop-dirname user-cache-directory)
(setq desktop-path (list user-cache-directory))

(setq-default display-fill-column-indicator-character 124)

(winner-mode 1)
(fringe-mode '(7 . 7))
;; (window-divider-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(undelete-frame-mode 1)

(setq-default help-at-pt-display-when-idle t
              help-at-pt-timer-delay 1)
(help-at-pt-set-timer) ;; hints in the echo area

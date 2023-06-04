;; general.el -*- lexical-binding: t; -*-

(setq use-short-answers t) ;; yes or no -> y or n 
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward)
(setq mouse-yank-at-point t)

(setq ring-bell-function #'ignore)
(setq visible-bell nil)
(setq initial-buffer-choice t)

(setq x-stretch-cursor t)
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)
(setq scroll-margin 10)
(setq scroll-conservatively 50)

(setq enable-recursive-minibuffers t)

(setq vc-follow-symlinks t) ;; no prompting for changing VC'd files

(setq-default indent-tabs-mode nil)

(setq revert-without-query '(".")) ;; no query for revert

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

(winner-mode)
;; (global-hl-line-mode)
(electric-pair-mode)
(fringe-mode '(3 . 0))
(vc-mode-line -1)

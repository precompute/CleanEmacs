;; -*- lexical-binding: t; -*-
(use-package compile
  :ensure nil
  :custom
  ;; `compilation-ask-about-save' saves all modified buffers when set to
  ;; nil.  Set `compilation-save-buffers-predicate' to a function that
  ;; returns nil so that `save-some-buffers' receives a nil for every
  ;; modified buffer it visits.  Hence, recompile will never ask to save
  ;; buffers, and will not save any buffers.
  (compilation-ask-about-save nil)
  (compilation-save-buffers-predicate (lambda () nil)))

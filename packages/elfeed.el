(use-package elfeed
  :ensure (:depth 1)
  :defer t
  :config
  (load-file (expand-file-name "elfeed.el" user-private-directory))
  (setq elfeed-db-directory (expand-file-name "elfeed/db/" user-cache-directory)
        elfeed-enclosure-default-dir (expand-file-name "elfeed/enclosures/" user-cache-directory))
  (make-directory elfeed-db-directory t)
  (make-directory elfeed-enclosure-default-dir t))

;;Uses elfeed-org to read feed list

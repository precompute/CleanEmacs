(use-package elfeed-org
  :elpaca (:depth 1)
  :after elfeed
  :preface
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-private-directory)))
  :init
  (elfeed-org))

(use-package magit
  :defer t
  :preface
  (setq magit-auto-revert-mode nil
	magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-log-margin '(t "%y%m%d %H%M%S" magit-log-margin-width t 15))
  :config
  (setq magit-repository-directories '(("~/.doom.d" . 0)
                                       ("~/.emacsclean" . 0)
                                       ("~/.emacs.one" . 0)
                                       ("~/46" . 1)
                                       ("~/temp/git/personal/t-e-r-m/" . 1)
                                       ("~/temp/git/t-e-r-m/" . 1)))
  (defun project-magit ()
    "Open magit-status in project root."
    (interactive)
    (magit-status (project-root (project-current t)))))

(setq-default left-fringe-width 15)

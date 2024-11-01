(use-package git-timemachine
  :elpaca (:depth 1)
  :config
  (defun toggle-git-timemachine-c ()
    (interactive)
    (if git-timemachine-mode
        (git-timemachine-mode (not git-timemachine-mode))
     ;; (let ((x olivetti-mode))
     ;;   (git-timemachine-mode)
     ;;   (olivetti-mode))
     )))

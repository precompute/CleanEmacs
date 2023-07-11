(use-package project
  :elpaca nil
  :defer t
  :bind
  (:map project-prefix-map
        ("T" . project-vterm)
        ("V" . project-magit)
        ("D" . project-dired))
  :config
  (defun project-dired ()
    "Dired in the current project's root directory."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (dired default-directory)))
  (dolist (project-command '((project-vterm "VTerm")
                             (project-magit "Magit")
                             (project-dired "Dired")))
    (add-to-list 'project-switch-commands project-command)))

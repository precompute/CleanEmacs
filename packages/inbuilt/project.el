(use-package project
  :ensure nil
  :defer t
  :bind
  (:map project-prefix-map
        ("T" . project-vterm)
        ("V" . project-magit)
        ("D" . project-dired)
        ("G" . project-consult-ripgrep))
  :config
  (defun project-dired ()
    "Dired in the current project's root directory."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (dired default-directory)))
  (defun project-consult-ripgrep ()
    "consult-ripgrep in the current project’s root directory."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (consult-ripgrep)))
  (dolist (project-command '((project-vterm "VTerm")
                             (project-magit "Magit")
                             (project-dired "Dired")
                             (project-consult-ripgrep "rg")))
    (add-to-list 'project-switch-commands project-command)))

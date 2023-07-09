(use-package vterm
  :elpaca (:depth 1)
  :config
  (defun project-vterm ()
    "Start VTerm in the current project's root directory.
  If a buffer already exists for running VTerm in the project's root,
  switch to it.  Otherwise, create a new VTerm buffer.
  With \\[universal-argument] prefix arg, create a new VTerm buffer even
  if one already exists."
    (interactive)
    (defvar vterm-bufname)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-bufname (project-prefixed-buffer-name "vterm"))
           (vterm-buf (get-buffer vterm-bufname)))
      (if (and vterm-buf (not current-prefix-arg))
          (pop-to-buffer vterm-buf (bound-and-true-p display-comint-buffer-action))
        (vterm)))))

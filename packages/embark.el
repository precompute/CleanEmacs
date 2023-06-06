(use-package embark
  :elpaca (:depth 1)
  :defer t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-;" . embark-act)
  (:map minibuffer-local-map
        ("C-;" . embark-act)
        ("C-c C-;" . embark-export)
        ("C-c C-l" . embark-collect)
        ("C-c C-e" . embark-export-write))
  ([remap describe-bindings] . embark-bindings)
  :config
  (defun embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (let* ((edit-command
            (pcase-let ((`(,type . ,candidates)
                         (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('file #'wdired-change-to-wdired-mode)
                ('consult-location #'occur-edit-mode)
                (x (user-error "embark category %S doesn't support writable export" x)))))
           (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
      (embark-export))))

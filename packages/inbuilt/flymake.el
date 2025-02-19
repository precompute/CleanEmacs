(use-package flymake
  :elpaca nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-mode-line-format nil)
  (setq python-flymake-command
        '("ruff" "--quiet" "--stdin-filename=stdin" "-")
        ;; "ruff --quiet check --preview --line-length=100 --output-format=pylint --select E --select W --stdin-filename=stdin -"
        ))

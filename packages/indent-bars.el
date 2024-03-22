(use-package indent-bars
  :elpaca ( :host github
            :repo "jdtsmith/indent-bars")
  ;; :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-pattern ". "
        indent-bars-width-frac 0.25
        indent-bars-pad-frac 0.2
        indent-bars-zigzag 0
        indent-bars-color-by-depth '( :blend 1
                                      :palette (font-lock-builtin-face
                                                font-lock-keyword-face
                                                mode-line
                                                font-lock-constant-face
                                                font-lock-string-face
                                                (cursor . 'bg)))
        indent-bars-highlight-current-depth '( :face 'region
                                               :pattern "."
                                               :pad 0.2
                                               :width 0.4
                                               :blend 0.5)
        indent-bars-treesit-support t
        indent-bars-no-descend-string t))

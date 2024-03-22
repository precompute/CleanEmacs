(use-package consult
  :custom
  (setq consult-ripgrep-args
        (string-clean-whitespace
         (concat
          (replace-regexp-in-string "--smart-case" ""
                                    (replace-regexp-in-string "--search-zip" "" consult-ripgrep-args))
          " --ignore-case")))
  (consult-async-split-style 'semicolon)
  :preface
  (global-set-key [remap bookmark-jump] #'consult-bookmark)
  (global-set-key [remap evil-show-marks] #'consult-mark)
  (global-set-key [remap evil-show-registers] #'consult-register)
  (global-set-key [remap goto-line] #'consult-goto-line)
  (global-set-key [remap imenu] #'consult-imenu)
  (global-set-key [remap Info-search] #'consult-info)
  (global-set-key [remap locate] #'consult-locate)
  (global-set-key [remap load-theme] #'consult-theme)
  (global-set-key [remap man] #'consult-man)
  (global-set-key [remap recentf-open-files] #'consult-recent-file)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
  (global-set-key [remap yank-pop] #'consult-yank-pop)
  :config
  (consult-customize consult-theme
                     :preview-key
                     '("C-SPC"
                       :debounce 0.5 any)))

(general-define-key
  [f5] 'delete-window
  [s-f5] 'delete-other-windows
  [f9] 'save-buffer
 
  "C-f" 'hippie-expand
 
  "s-h" 'evil-backward-char
  "s-l" 'evil-forward-char
  "s-j" 'evil-next-line
  "s-k" 'evil-previous-line
  )

(general-create-definer evil-keymaps-c
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "s-SPC")

(evil-keymaps-c
  "SPC" 'project-find-file
  "f"  '(:ignore t :wk "file")
  "ff" 'find-file
  "fr" 'recentf-open

  "g" '(:ignore t :wk "magit")
  "gg" 'magit-status

  "b" '(:ignore t :wk "buffer")
  "br" 'revert-buffer
  "bb" 'switch-to-buffer
  "bB" 'switch-to-buffer-other-window
  "bp" 'previous-buffer
  "bn" 'next-buffer
  
  "s" '(:ignore t :wk "search")
  "s_" 'consult-keep-lines
  "s-" 'consult-focus-lines
  "ss" 'consult-line
  ;; "sp"
  ;; "sd"

  "RET" 'consult-bookmark
  
  "i" '(:ignore t :wk "insert")
  "iy" 'yank-pop
  
  "o" '(:ignore t :wk "other")
  "oo" 'dired-jump

  "h" '(:ignore t :wk "help")
  "hv" 'describe-variable
  "hf" 'describe-function
  "he" 'view-echo-area-messages
  "hl" 'view-lossage
  "hk" 'describe-key
  "ht" 'load-theme
  "hb" 'describe-bindings

  "q" '(:ignore t :wk "quit")
  "qq" 'save-buffers-kill-terminal
  )

(general-define-key
  :states 'normal
  "z" '(:ignore t :wk "end")
  "zx" 'kill-current-buffer)

(general-define-key
  :states 'normal
  "C-w" '(:ignore t)
  "C-w C-u" 'winner-undo
  "C-w u" 'winner-undo
  "C-w C-r" 'winner-redo
  "C-w r" 'winner-redo)

;;;; General
;;;;; Global nice-to-haves
(general-define-key
  [f5] 'delete-window
  [s-f5] 'delete-other-windows
  [f6] 'timestamp
  [f9] 'save-buffer
  [f10] 'save-and-delete-window
  [f12] 'save-and-kill-buffer

  "C-f" 'hippie-expand

  "s-h" 'evil-backward-char
  "s-l" 'evil-forward-char
  "s-j" 'evil-next-line
  "s-k" 'evil-previous-line)

;;;;; Evil-Mode
;;;;;; Leader
(general-create-definer evil-keymaps-c
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "s-SPC")

;;;;;; Keys under Evil-mode Leader
(evil-keymaps-c
  "SPC" 'project-find-file
  "S-SPC" 'project-switch-project
  "f"  '(:ignore t :wk "file")
  "ff" 'find-file
  "fr" 'recentf-open
  "fD" 'make-directory

  "g" '(:ignore t :wk "Version Control")
  "gg" 'magit-status
  "gL" 'magit-log-buffer-file
  "g[" 'diff-hl-previous-hunk
  "g]" 'diff-hl-next-hunk
  "gr" 'diff-hl-revert-hunk
  "g+" 'evil-numbers/inc-at-pt
  "g-" 'evil-numbers/dec-at-pt

  "b" '(:ignore t :wk "buffer")
  "br" 'revert-buffer
  "bb" 'switch-to-buffer
  "bB" 'switch-to-buffer-other-window
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bi" 'ibuffer-other-window

  "s" '(:ignore t :wk "show")
  "s_" 'consult-keep-lines
  "s-" 'consult-focus-lines
  "ss" 'consult-line
  "sS" 'consult-line-multi
  "si" 'consult-imenu
  "sm" 'consult-global-mark
  "sd" 'consult-ripgrep-local
  "sp" 'consult-ripgrep
  "se" '(:ignore t :wk "evil-show")
  "sem" 'evil-show-marks
  "sej" 'evil-show-jumps
  "ser" 'evil-show-registers

  "TAB" '(:ignore t :wk "perspective")
  "TAB TAB" 'persp-switch
  "TAB d" 'persp-kill
  "TAB r" 'persp-rename
  "TAB n" 'persp-next
  "TAB p" 'persp-prev
  "TAB a" 'persp-add-buffer
  "TAB A" 'persp-set-buffer
  "TAB b" 'persp-switch-to-buffer
  "TAB a" 'persp-remove-buffer

  "RET" 'consult-bookmark

  "'" 'vertico-repeat

  "i" '(:ignore t :wk "insert")
  "iy" 'consult-yank-from-kill-ring
  "ii" 'kill-new-from-global-paste-c

  "o" '(:ignore t :wk "other")
  "oo" 'dired-jump
  ;; "od" 'dired-jump
  "of" 'make-frame
  "oi" 'imenu-list-smart-toggle
  "op" 'dired-sidebar-toggle-sidebar
  "o'" 'vertico-repeat
  "o\"" 'vertico-repeat-select

  "t" '(:ignore t :wk "toggle")
  "tl" 'display-line-numbers-mode
  "ti" 'highlight-indent-guides-mode
  "tf" 'follow-mode
  "tc" 'corfu-mode
  "teb" 'toggle-eldoc-box

  "c" '(:ignore t :wk "code diagnostics")
  "cr" 'flymake-show-buffer-diagnostics

  "!" '(:ignore t :wk "exclaim")
  "!R" 'make-window-larger-c
  ;; "!_" 'flip-frame
  ;; "!|" 'flop-frame
  "!?" 'transpose-frame
  ;; "!>" 'rotate-frame-clockwise
  ;; "!<" 'rotate-frame-anticlockwise
  ;; "!f" 'follow-mode

  "h" '(:ignore t :wk "help")
  "hv" 'describe-variable
  "hf" 'describe-function
  "he" 'view-echo-area-messages
  "hl" 'view-lossage
  "hk" 'describe-key
  "ht" 'load-theme
  "hb" 'describe-bindings
  "h'" 'describe-char
  "hF" 'describe-face
  "hm" 'describe-mode
  "hi" 'info

  "q" '(:ignore t :wk "quit")
  "qq" 'save-buffers-kill-terminal
  "qf" 'delete-frame)

;;;;;; Sans leader
;;;;;;; Buffer
(general-define-key
  :states 'normal
  "z" '(:ignore t :wk "end")
  "zp" 'delete-window
  "zs" 'save-buffer
  "zx" 'kill-current-buffer
  "zZ" 'bury-buffer)

;;;;;;; Undo / Redo
(general-define-key
  :states 'normal
  "C-w" '(:ignore t)
  "C-w C-u" 'winner-undo
  "C-w u" 'winner-undo
  "C-w C-r" 'winner-redo
  "C-w r" 'winner-redo)

;;;;;;; Nav/other
(general-define-key
  :states 'normal
  :keymaps 'emacs-lisp-mode-map
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

;;;;;;; bicycle-cycle
(general-define-key
 :states 'normal
 :keymaps 'outline-minor-mode-map
 [tab] 'bicycle-cycle
 [backtab] 'bicycle-cycle-global)

;;;;;;; misc
(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "_" 'locate-git-file-c)

;;;;;;; Evil operators
(general-define-key
  :states 'normal
  "g" '(:ignore t)
  "g c" 'evilnc-comment-operator
  "g g" 'evil-goto-first-line
  "g e" 'evil-operator-eval-region)

(general-define-key
 :states 'motion
 "a g" 'evil-textobj-whole-buffer
 "a f" 'evil-textobj-get-func)

;;;;;;; Misc/Custom
(general-define-key
  :states 'normal
  "C-H-s--" 'flip-frame
  "C-H-s-\\" 'flop-frame
  "C-H-s-/" 'transpose-frame
  "C-H-s-." 'rotate-frame-clockwise
  "C-H-s-," 'rotate-frame-anticlockwise
  "C-s-h" 'evil-window-decrease-width
  "C-s-j" 'evil-window-decrease-height
  "C-s-k" 'evil-window-increase-height
  "C-s-l" 'evil-window-increase-width
  "H-s-'" 'delete-other-windows
  "s-[" 'outline-previous-heading
  "s-]" 'outline-next-heading
  "H-c" 'git-auto-time-commit
  "H-C" 'git-prompt-commit
  "H-r" 'refile3-main-transient
  "\\" 'repeat
  "C-/" 'toggle-modes-transient-c

  "<backspace>" 'scroll-up-command
  "S-<backspace>" 'scroll-down-command)

(general-define-key
  :states 'insert
  "C-S-u" 'insert-char-5-discard-end)

;;;; Evil
;;;;; ge (eval) Operator

(evil-define-operator evil-operator-eval-region (beg end)
  "Evaluate selection."
  :move-point nil
  (interactive "<r>")
  (eval-region beg end))

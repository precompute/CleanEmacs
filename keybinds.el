;;;; General
;;;;; Global nice-to-haves
;; (global-set-key (kbd "C-i") nil)
;; (define-key evil-normal-state-map (kbd "C-i") nil)
;; (define-key evil-normal-state-map (kbd "<tab>") nil)
(general-define-key
 [f5] 'delete-window
 [s-f5] 'delete-other-windows
 [f6] 'timestamp
 [s-f6] 'timestamp-no-time
 [f9] 'save-buffer
 [f10] 'save-and-delete-window
 [f12] 'save-and-kill-buffer

 "C-<tab>" 'hippie-expand-small
 "C-<iso-lefttab>" 'hippie-unexpand
 "C-f" 'hippie-expand-all

 "s-h" 'evil-backward-char
 "s-l" 'evil-forward-char
 "s-j" 'evil-next-line
 "s-k" 'evil-previous-line

 "<mouse-8>" 'previous-buffer
 "<mouse-9>" 'next-buffer

 "<header-line> <mouse-2>" 'delete-window
 "<header-line> <mouse-3>" 'toggle-frame-fullscreen)

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
  "C-SPC" 'consult-fd-local
  "f"  '(:ignore t :wk "file")
  "ff" 'find-file
  "fr" 'consult-recent-file
  "fR" 'recentf-open-files
  "fD" 'make-directory

  "g" '(:ignore t :wk "Version Control")
  "gg" 'magit-status
  "gG" 'magit-dispatch
  "gF" 'magit-file-dispatch
  "gl" 'magit-log-buffer-file
  "g*" 'magit-list-repositories
  "gb" 'magit-branch-checkout
  "gB" 'magit-blame-addition
  "g[" 'diff-hl-previous-hunk
  "g]" 'diff-hl-next-hunk
  "gr" 'diff-hl-revert-hunk
  "gs" 'diff-hl-stage-current-hunk
  "gh" 'diff-hl-diff-goto-hunk
  "gp" 'consult-git-grep
  "g+" 'evil-numbers/inc-at-pt
  "g-" 'evil-numbers/dec-at-pt
  "gt" 'git-timemachine

  "b" '(:ignore t :wk "buffer")
  "br" 'revert-buffer
  "bb" 'switch-to-buffer
  "bB" 'switch-to-buffer-other-window
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bi" 'ibuffer-other-window
  "bc" 'clone-indirect-buffer-other-window

  "s" '(:ignore t :wk "show")
  "s_" 'consult-keep-lines
  "s-" 'consult-focus-lines
  "ss" 'consult-line
  "sS" 'consult-line-multi
  "si" 'consult-outline
  "sI" 'consult-imenu
  "s C-i" 'consult-imenu-multi
  "sm" 'evil-show-marks
  "sM" 'consult-global-mark
  "s C-m" 'evil-collection-consult-jump-list
  "sP" 'consult-ripgrep-local
  "sp" 'consult-ripgrep
  "s C-h i" 'consult-info
  "sr" 'evil-show-registers

  "s" '(:ignore t :wk "show-at-point")
  "Sf" 'find-file-at-point
  "Sp" 'consult-ripgrep-at-point-c
  "SP" 'consult-ripgrep-local-at-point-c
  "Ss" 'consult-line-at-point-c
  "S SPC" 'consult-fd-local-global-at-point-c
  "S C-SPC" 'consult-fd-local-at-point-c
  "Sh" 'highlight-at-point-c
  "SH" 'unhighlight-at-point-c
  "S C-h" 'unhighlight-all-c

  "TAB" '(:ignore t :wk "perspective")
  "TAB TAB" 'persp-switch
  "TAB d" 'persp-kill
  "TAB r" 'persp-rename
  "TAB n" 'persp-next
  "TAB p" 'persp-prev
  "TAB a" 'persp-add-buffer
  "TAB A" 'persp-set-buffer
  "TAB b" 'persp-switch-to-buffer
  "TAB x" 'persp-remove-buffer

  "RET" 'consult-bookmark

  "'" 'vertico-repeat

  "i" '(:ignore t :wk "insert")
  "iy" 'consult-yank-from-kill-ring
  "ii" 'kill-new-from-global-paste-c

  "o" '(:ignore t :wk "other")
  "oo" 'dired-jump
  "oO" 'dired-jump-other-window
  ;; "od" 'dired-jump
  "of" 'make-frame
  "oF" 'tear-off-window
  "oi" 'imenu-list-smart-toggle
  "op" 'dired-sidebar-toggle-sidebar
  "o'" 'vertico-repeat
  "o\"" 'vertico-repeat-select
  "o-" 'echo-current-buffer-path
  "o RET" 'echo-current-time
  "ot" 'vterm
  "oT" 'project-vterm
  "os" 'scratchiest

  "t" '(:ignore t :wk "toggle")
  "tl" 'display-line-numbers-mode
  "ti" 'highlight-indent-guides-mode
  "tf" 'follow-mode
  "tc" 'corfu-mode
  "tC" 'corfu-toggle-autocomplete
  "teb" 'toggle-eldoc-box

  "c" '(:ignore t :wk "code diagnostics")
  "cr" 'flymake-show-buffer-diagnostics
  "cR" 'consult-flymake
  "cC" 'compile
  "cc" 'recompile
  "cE" 'load-current-file

  "w" '(:ignore t :wk "window")
  "wm" 'rotate-frame-anticlockwise
  "wi" 'rotate-frame-clockwise
  "wr" 'flip-frame
  "ws" 'flop-frame
  "wk" 'transpose-frame
  "wn" 'rotate-frame
  "we" 'rotate-frame
  "wj" 'windmove-swap-states-left
  "wl" 'windmove-swap-states-down
  "wu" 'windmove-swap-states-up
  "ww" 'split-root-window-below
  "wW" 'split-root-window-right
  "wy" 'windmove-swap-states-right

  "!" '(:ignore t :wk "exclaim")
  "!R" 'make-window-larger-c
  "!?" 'transpose-frame

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
  "hI" 'consult-info
  "hD" 'toggle-debug-mode
  "hds" 'profiler-start
  "hdS" 'profiler-stop-and-report
  "hM" 'woman
  "h C-f" 'elisp-show-callable-definition-c
  "h C-v" 'elisp-show-variable-definition-c
  ;; `elisp-slime-nav-describe-elisp-thing-at-point` is not adequate

  "d" '(:ignore t :wk "other")
  "ds" 'lexic-search-word-at-point
  "dS" 'lexic-search

  "m" '(:ignore t :wk "mark")
  "ml" 'whack-a-thing-whack-line
  "mL" 'whack-a-thing-quick-whack-line
  "mw" 'whack-a-thing-whack-word
  "mW" 'whack-a-thing-transient

  "q" '(:ignore t :wk "quit")
  "qq" 'save-buffers-kill-terminal
  "qQ" 'clean-exit
  "qf" 'delete-frame-force-c
  "qF" 'undelete-frame
  "qr" 'restart-emacs)

;;;;;; Sans leader
;;;;;;; Buffer
(general-define-key
 :states 'normal
 "z" '(:ignore t :wk "end")
 "zp" 'delete-window
 "zs" 'save-buffer
 "zx" 'kill-current-buffer
 "zX" 'kill-current-buffer-and-window-c
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

;;;;;;; misc
(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "_" 'locate-git-file-c)

(general-define-key
 :states 'normal
 :keymaps '(elfeed-show-mode-map
            elfeed-search-mode-map)
 "C-/" 'elfeed-transient-c)

;;;;;;; Evil operators
(general-define-key
 :states 'normal
 "g" '(:ignore t)
 "g c" 'evilnc-comment-operator
 "g g" 'evil-goto-first-line
 "g e" 'evil-operator-eval-region
 "g p" 'evil-reselect-paste)

(general-define-key
 :states 'motion
 "a g" 'evil-textobj-whole-buffer
 "a h" 'evil-textobj-visible-lines
 "a f" 'evil-textobj-get-func
 "a l" 'evil-textobj-entire-line
 "a N P" 'evil-textobj-forward-until-empty-line
 "a P P" 'evil-textobj-backward-until-empty-line)

;;;;;;; loccur
(general-define-key
 :states 'normal
 "C-s -" 'loccur-no-highlight
 "C-s C--" 'loccur-isearch
 "C-s _" 'loccur-current)

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
 "\\" 'repeat
 "-" 'repeat
 "C-/" 'toggle-modes-transient-c

 "H-a" 'org-capture

 "H-h c" 'git-auto-time-commit
 "H-h C" 'git-prompt-commit
 "H-h r" 'refile3-main-transient

 "C-S-h h" 'enable-hyper-key-c

 "<backspace>" 'scroll-up-command
 "S-<backspace>" 'scroll-down-command

 "g n" 'narrow-to-page
 "g N" 'widen

 "C-S-t" 'xref-go-forward

 "C-d" 'scroll-5l-up
 "C-S-d" 'scroll-5l-down)

(general-define-key
 :states 'insert
 "C-S-u" 'insert-char-5-discard-end
 "M-o" 'evil-open-below
 "M-O" 'evil-open-above)

(general-define-key
 :states 'visual
 "<" 'evil-shift-left-c
 ">" 'evil-shift-right-c
 "C-x C-e" 'eval-region
 "g n" 'narrow-to-region
 "g N" 'widen
 "u" 'undo-with-prefix
 "C-r" 'undo-redo-with-prefix)

(general-define-key
 :keymaps '(org-mode-map prog-mode-map)
 :states 'insert
 "H-<return>" 'continue-structure-c)

(general-define-key
 :keymaps 'org-mode-map
 :states '(visual normal)
 ">" 'evil-org->
 "<" 'evil-org-<)

(general-define-key
 :keymaps 'org-mode-map
 :states 'insert
 "TAB" 'org-cycle)

(general-define-key
 :keymaps 'org-mode-map
 "H-q i" 'org-id-create-insert
 "H-b q" 'org-insert-block-quote-c
 "H-b s" 'org-insert-block-src-c
 "H-b Q" 'org-insert-block-quote-meta-c
 "H-b S" 'org-insert-block-src-meta-c
 "H-b b" 'org-insert-block-custom-c
 "H-b B" 'org-insert-block-custom-meta-c)

(general-define-key
 :keymaps 'Info-mode-map
 "H-j" 'Info-history-back
 "H-y" 'Info-history-forward
 "H-S-j" 'Info-history-back-menu
 "H-S-y" 'Info-history-forward-menu
 "H-i" 'Info-next
 "H-m" 'Info-prev
 "H-e" 'Info-up
 "H-s" 'consult-info)

;; c x p a q b z h
(general-define-key
 :states '(normal insert visual)
 "H-c" 'clipboard-kill-ring-save
 "H-x" 'clipboard-kill-region
 "H-p" 'clipboard-yank)

(general-define-key
 :states '(normal insert visual)
 "C-i" 'evil-jump-backward
 "C-o" 'evil-jump-forward)

(general-define-key
 :states 'normal
 "C-z" 'legit-to-line
 "C-S-z" 'legit-from-line)

(general-define-key
 :keymaps 'vertico-map
 "<next>" 'vertico-scroll-up
 "<prior>" 'vertico-scroll-down
 "C-<next>" 'next-history-element
 "C-<prior>" 'previous-history-element)

;; SPC-M-SPC to select a file from all files
;; visual select all visible lines
;; highlight all matching under point
;; search with under point
;; evil jump
;; Bicycle: restore point after

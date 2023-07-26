;; packages/evil.el -*- lexical-binding: t; -*-
(use-package evil
  :config
  (if (boundp 'undo-tree-mode)
      (setq evil-undo-system 'undo-tree
            evil-undo-function #'undo-tree-undo
            evil-redo-function #'undo-tree-redo)
    (if (boundp 'undo-fu-mode)
        (setq evil-undo-system 'undo-fu
              evil-undo-function #'undo-fu-only-undo
              evil-redo-function #'undo-fu-only-redo)
      (setq evil-undo-system 'undo-redo ;; 'undo-fu
            evil-undo-function #'undo
            evil-redo-function #'undo-redo)))
  (setq evil-split-window-below t
        evil-vsplit-window-right t)
  (setq evil-symbol-word-search t
        evil-shift-width 2)
  (setq evil-want-Y-yank-to-eol t ;; Doesn’t work
        evil-want-C-g-bindings t
        evil-want-C-i-jump t
        evil-respect-visual-line-mode t)
  (setq evil-insert-state-cursor '(hbar . 1)
        evil-normal-state-cursor '(hbar . 4)
        evil-visual-state-cursor 'hbar
        evil-replace-state-cursor 'hollow
        evil-motion-state-cursor 'hollow
        evil-operator-state-cursor 'hollow
        evil-emacs-state-cursor 'bar)
  (evil-put-command-property 'evil-yank-line :motion 'evil-end-of-line-or-visual-line) ;; workaround for evil-want-Y-yank-to-eol

  (evil-define-text-object evil-textobj-whole-buffer (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))

  (evil-define-text-object evil-textobj-get-func (count &optional _beg _end type)
    "Text object to select the top-level Lisp form or function definition at
point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end type)))

  (evil-define-text-object evil-textobj-entire-line (count &optional _beg _end type)
    "Text object for the current line"
    (evil-range (pos-bol) (pos-eol) type))

  (evil-define-text-object evil-textobj-forward-until-empty-line (count &optional _beg _end type)
    "Text object until the end of the current paragraph"
    (evil-range (point) (re-search-forward "\\(?:$\\)\\(?:^\\)[[:space:]]*$") type))

  (evil-define-text-object evil-textobj-backward-until-empty-line (count &optional _beg _end type)
    "Text object until the start of the current paragraph"
    (evil-range (point) (re-search-backward "\\(?:$\\)\\(?:^\\)[[:space:]]*$") type))

  (evil-define-operator evil-operator-eval-region (beg end)
    "Evaluate selection."
    :move-point nil
    (interactive "<r>")
    (eval-region beg end))

  (defun evil-reselect-paste ()
    "Return to visual mode and reselect the last pasted region.
Copied from Doom’s +evil/reselect-paste."
    (interactive)
    (cl-destructuring-bind (_ _ _ beg end &optional _) evil-last-paste
      (evil-visual-make-selection (save-excursion (goto-char beg) (point-marker)) end)))

  (defun evil-shift-left-c ()
    "Disable disabling visual selection after shifting left."
    (interactive)
    (call-interactively #'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

  (defun evil-shift-right-c ()
    "Disable disabling visual selection after shifting right."
    (interactive)
    (call-interactively #'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))

  (evil-define-operator evil-org-> (beg end count)
    "Demote, indent, move column right.
In items or headings, demote heading/item.
In code blocks, indent lines
In tables, move column to the right."
    :move-point nil
    (interactive "<r><vc>")
    (when (null count) (setq count 1))
    (cond
     ;; Work with subtrees and headings
     ((org-with-limited-levels
       (or (org-at-heading-p)
           (save-excursion (goto-char beg) (org-at-heading-p))))
      (if (> count 0)
          (org-map-region 'org-do-demote beg end)
        (org-map-region 'org-do-promote beg end)))
     ;; Shifting table columns
     ((and (org-at-table-p)
           (save-excursion
             (goto-char beg)
             (<= (line-beginning-position) end (line-end-position))))
      (evil-org-table-move-column beg end count))
     ;; Work with items
     ((and (org-at-item-p)
           (<= end (save-excursion (org-end-of-item-list))))
      (evil-org-indent-items beg end count))
     ;; Default indentation
     (t
      ;; special casing tables
      (when (and (not (region-active-p)) (org-at-table-p))
        (setq beg (min beg (org-table-begin)))
        (setq end (max end (org-table-end))))
      (evil-shift-right beg end count)))
    (when (and evil-org-retain-visual-state-on-shift (evil-visual-state-p))
      (evil-normal-state)
      (evil-visual-restore)))

  (evil-define-operator evil-org-< (beg end count)
    "Promote, dedent, move column left.
In items or headings, promote heading/item.
In code blocks, indent lines
In tables, move column to the left."
    (interactive "<r><vc>")
    (evil-org-> beg end (- (or count 1))))

  (defun evil-org-indent-items (beg end count)
    "Indent all selected items in itemlist.
Argument BEG Begin of subtree items to indent.
Argument END End of subtree items to indent.
Argument COUNT if negative, items are dedented instead."
    (when (null count) (setq count 1))
    (let* ((struct (save-excursion (goto-char beg) (org-list-struct)))
           (region-p (region-active-p)))
      ;; special case: indenting all items
      (if (and struct org-list-automatic-rules (not region-p)
               (= (point-at-bol) (org-list-get-top-point struct)))
          (org-list-indent-item-generic count nil struct)
        ;; indenting selected items
        (save-excursion
          (when region-p (deactivate-mark))
          (set-mark beg)
          (goto-char end)
          (org-list-indent-item-generic count t struct)))))

  (defun evil-org-table-move-column (beg end arg)
    "Move org table column.
Argument BEG, first column
Argument END, second column
If ARG > 0, move column BEG to END.
If ARG < 0, move column END to BEG"
    (let* ((text (buffer-substring beg end))
           (n-cells-selected (max 1 (count ?| text)))
           (n-columns-to-move (* n-cells-selected (abs arg)))
           (move-left-p (< arg 0)))
      (goto-char (if move-left-p end beg))
      (dotimes (_ n-columns-to-move) (org-table-move-column move-left-p))))

  :init
  (setq evil-want-keybinding nil)
  (evil-mode))


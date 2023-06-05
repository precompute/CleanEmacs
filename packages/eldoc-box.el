(use-package eldoc-box
  :elpaca (:depth 1)
  :hook (prog-mode . eldoc-box-hover-mode)
  :custom-face
  ;; (eldoc-box-body
  ;;  ((t (:inherit variable-pitch))))
  (eldoc-box-border
   ;; ((t (:background ,(face-attribute 'font-lock-builtin-face :foreground)))))
   ((t (:background ,(face-attribute 'shadow :foreground)))))
  :config
  (defun eldoc-box-bottom-left-position-function (width height)
    "Set childframe position to bottom left."
    (pcase-let ((`(,offset-l ,offset-r ,offset-t) eldoc-box-offset))
      (cons offset-l
            (- (frame-outer-height (selected-frame)) height offset-t))))
  (defun eldoc-box-add-padding ()
    "Add padding to the box by adding spaces before every line and
newlines on top and bottom."
    (save-excursion
      (goto-char (point-min))
      (while (not (eq (point-max) (point))) ;; Had to brute-force it,
        (goto-char (pos-bol)) (insert "  ") ;; regex wasn’t working.
        (goto-char (pos-eol)) (insert "  ")
        (goto-char (+ 1 (point))))
      ;; (re-search-forward (rx (or (seq bol)))) (replace-match "AA")
      ;; (goto-char (point-max))
      ;; (re-search-backward (rx (seq eol ))) (replace-match "  ")
      (goto-char (point-min)) (re-search-forward (rx (seq bos))) (replace-match "\n")
      (goto-char (point-min)) (re-search-forward (rx (seq eos))) (replace-match "\n \n")))
  (setq eldoc-box-only-multi-line t
        eldoc-box-cleanup-interval 0.1
        eldoc-box-offset '(10 10 60)
        eldoc-box-position-function #'eldoc-box-bottom-left-position-function)
  (remove-hook 'eldoc-box-buffer-hook #'eldoc-box--condense-large-newline-gaps)
  (add-hook 'eldoc-box-buffer-hook #'eldoc-box-add-padding))

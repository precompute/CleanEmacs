(use-package hippie-exp
  :ensure nil
  ;; Variable for buffers that require a certain completion function.
  (defvar-local hippie-expand-literature-buffer nil)

  (defun hippie-expand-all (arg)
    "`hippie-expand’ with all try-functions.
ARG passed to `hippie-expand’."
    (interactive "P")
    (let ((hippie-expand-try-functions-list
           '( try-complete-file-name try-complete-file-name-partially
              try-complete-lisp-symbol try-complete-lisp-symbol-partially
              try-expand-all-abbrevs
              try-expand-dabbrev try-expand-dabbrev-all-buffers
              try-expand-dabbrev-from-kill try-expand-dabbrev-visible
              try-expand-list try-expand-list-all-buffers
              try-expand-whole-kill)))
      (hippie-expand arg)))

  (defun hippie-unexpand ()
    "`hippie-expand’ with negative arg."
    (interactive)
    (hippie-expand -1))

  (defun hippie-expand-small (arg)
    "`hippie-expand’ with a selected repertoire of try-functions.
ARG passed to `hippie-expand’."
    (interactive "P")
    (with-current-buffer (current-buffer)
      (let* ((hippie-expand-try-functions-list
              '(try-expand-dabbrev try-expand-dabbrev-visible)))
        (when (memq major-mode '( emacs-lisp-mode lisp-interaction-mode))
          (setq hippie-expand-try-functions-list
                (append hippie-expand-try-functions-list
                        '( try-complete-lisp-symbol-partially try-complete-lisp-symbol
                           try-expand-dabbrev-all-buffers))))
        (when (or (derived-mode-p 'prog-mode)
                  (and (memq major-mode '( org-mode markdown-mode))
                       (not hippie-expand-literature-buffer)))
          (setq hippie-expand-try-functions-list
                (append hippie-expand-try-functions-list
                        '( try-complete-file-name-partially try-complete-file-name
                           try-expand-dabbrev-all-buffers))))
        (when (eq t hippie-expand-literature-buffer)
          (message "in a literature buffer")
          (setq hippie-expand-try-functions-list
                (reverse
                 (append hippie-expand-try-functions-list '(try-complete-word-dict)))))
        (pp hippie-expand-try-functions-list)
        (hippie-expand arg))))

  (defun try-complete-word-dict (old)
    "Function for `hippie-expand’.  Try to complete word from dict.
Only works at the end of a word!  OLD is t on "
    (unless old
      (he-init-string (- (point) (length (current-word))) (point))
      (unless (he-string-member he-search-string he-tried-table)
        (add-to-list 'he-tried-table he-search-string))
      (if (string-blank-p he-search-string)
          (setq he-expand-list ())
        (setq he-expand-list (ispell-lookup-words (concat he-search-string "*")))))
    (while (and he-expand-list (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (when old (he-reset-string)) ())
      (he-substitute-string (car he-expand-list))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t)))

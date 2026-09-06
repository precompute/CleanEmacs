;; -*- lexical-binding: t; -*-
(use-package hippie-exp
  :ensure nil
  :config
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
        (setq he-expand-list
              (if jinx-mode
                  (mapcar #'substring-no-properties (jinx--correct-suggestions he-search-string))
                (ispell-lookup-words (concat he-search-string "*"))))))
    (while (and he-expand-list (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (when old (he-reset-string)) ())
      (he-substitute-string (car he-expand-list))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t))

  (defvar-local hippie-expand-capf--fn-list
    '( try-complete-lisp-symbol try-complete-lisp-symbol-partially
       try-expand-all-abbrevs try-expand-whole-kill
       try-complete-file-name try-complete-file-name-partially)
    "List of functions for `hippie-expand-capf'.")

  (defvar-local hippie-expand-capf--limit 15
    "Number of candidates from every function in fn-list for `hippie-expand-capf'.")

  (defun hippie-expand-capf--collect (fn &optional limit)
    "Collect LIMIT candidates from FN.
LIMIT is defined by `hippie-expand-capf--limit'."
    (let ((c nil))
      ;; prevent hippie-expand functions from writing to the buffer.
      ;; Collect candidates instead.
      (cl-letf (((symbol-function #'he-substitute-string)
                 (lambda (z &rest _) (push z c))))
        (cl-loop with limit = (or limit hippie-expand-capf--limit)
                 for i from 0 while (and (funcall fn (> i 0)) (< i limit))
                 finally return nil))
      (nreverse c)))

  (defun hippie-expand-capf (&optional fn-list)
    "Completion-at-point function with Hippie-Expand candidates.
We dynamically provide all (?) candidates from functions in FN-LIST.
NOTE: Very Expensive!"
    (interactive)
    (let* ((we (point))
           (wb (+ we (save-excursion (skip-syntax-backward "w_"))))
           ;; not using `bounds-of-thing-at-point', this is more general.
           ;; (feels a little hacky with the `save-excursion'.)
           ;; See Info Manual "Syntax Class Table".
           (z '())
           (he-tried-table nil) ;; Can be non-nil on invocation.  Would be a problem if interleaved with hippie-expand.
           (fn-list (or fn-list hippie-expand-capf--fn-list)))
      (save-excursion
        (save-restriction
          (dolist (fn fn-list)
            (dolist (c (hippie-expand-capf--collect fn))
              (when (= (marker-position he-string-beg) wb)
                (cl-pushnew c z))))))
      (when z (list wb we (nreverse z) :exclusive 'no))))

  (defun completion-at-point-hippie-capf (&optional fn-list limit)
    "Call `completion-at-point' with `hippie-expand-capf' as the only function in `completion-at-point-functions'."
    (interactive)
    (let ((completion-at-point-functions (list (lambda (&rest _) (hippie-expand-capf fn-list)) t))
          (hippie-expand-capf--limit (or limit hippie-expand-capf--limit)))
      (funcall-interactively #'completion-at-point)))

  (defun completion-at-point-hippie-word-capf ()
    "Call `completion-at-point-hippie-capf' with `try-complete-word-dict' as the only function."
    (interactive)
    (completion-at-point-hippie-capf '(try-complete-word-dict) 100)))

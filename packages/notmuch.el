(use-package notmuch
  :ensure (:depth 1)
  :config
  (defun notmuch-async-poll-all ()
    "Call notmuch poll via `async-shell-command’.
The poll function that ships with the notmuch package blocks emacs."
    (interactive)
    (message "Running \"notmuch new\".")
    (let ((display-buffer-alist (cons (list (regexp-quote shell-command-buffer-name-async)
                                            #'display-buffer-no-window)
                                      display-buffer-alist)))
      (async-shell-command "notmuch new")))

  (defun notmuch-delete-window-c ()
    "Advice that deletes active window after `notmuch-bury-or-kill-this-buffer’."
    (delete-window))
  (advice-add 'notmuch-bury-or-kill-this-buffer :after #'notmuch-delete-window-c))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :config
  (global-set-key [remap comment-line] #'evilnc-comment-or-uncomment-lines))

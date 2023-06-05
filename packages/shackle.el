(use-package shackle
  :config
  (setq shackle-rules '((magit-status-mode :size 0.5  :align right :select t)
                        (ilist-mode        :size 0.2  :align left  :select nil)
                        (dired-sidebar     :size 0.3  :align left  :select t)
                        (helpful-mode      :size 0.25 :align below :select t))
        shackle-default-rule '(:select t :size 0.3 :align below))
  :init
  (shackle-mode))

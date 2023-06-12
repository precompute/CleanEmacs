(use-package org-agenda
  :elpaca nil
  :defer t
  :bind
  (:map org-agenda-mode-map
        ("C-/" . org-agenda-transient)
        ("<tab>" . nil)
        ("<backtab>" . nil)
        ("<return>" . nil)
        ("<return>" . org-agenda-goto)
        ("<tab>" . origami-toggle-node)
        ("<backtab>" . origami-toggle-all-nodes))
  :custom
  (org-agenda-include-deadlines t)
  (org-agenda-overriding-header " ")
  (org-agenda-tags-column 'auto)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-with-follow-mode nil)
  (org-agenda-log-mode-add-notes nil)
  (org-agenda-loop-over-headlines-in-active-region t)
  (org-agenda-view-columns-initially nil)
  (org-agenda-columns-show-summaries t)
  (org-agenda-columns-compute-summary-properties t)
  (org-agenda-current-time-string "~")
  (org-agenda-block-separator nil)
  (org-agenda-search-headline-for-time nil)
  (org-agenda-use-time-grid nil)
  (org-agenda-time-grid '((weekly today require-timed) nil "" ""))
  :config
  (org-agenda-refresh-c)
  (dolist (hook '(org-agenda-refresh-c
                  (lambda () (setq org-agenda-ss-seq 0))
                  (lambda () (setq org-agenda-ss-seq-2 0))
                  (lambda () (setq org-agenda-ss-seq-3 0))
                  org-super-agenda-mode
                  outline-minor-mode))
    (add-hook 'org-agenda-mode-hook hook)))

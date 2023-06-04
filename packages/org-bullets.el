(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("●" "○" "◊" "◇" "◆")))

(use-package org-appear
  :elpaca (:depth 1)
  :hook org-mode
  :config
  (setq ;; org-appear-elements '(bold italic underline strike-through verbatim code subscript superscript entity link keyword)
        org-appear-autoentities t
        org-appear-autosubmarkers t
        org-appear-autokeywords t
        org-appear-autolinks t))

(use-package org-sliced-images
  :elpaca (:depth 1)
  :defer t
  :config
  (add-hook 'org-mode-hook #'org-sliced-images-display-inline-images))

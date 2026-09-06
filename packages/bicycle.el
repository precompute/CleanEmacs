;; -*- lexical-binding: t; -*-
(use-package bicycle
  :ensure t
  :bind (:map prog-mode-map
              ("M-<tab>" . bicycle-cycle)
              ("M-<iso-lefttab>" . bicycle-cycle-global)))

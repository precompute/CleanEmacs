(use-package cheese-capture
  :ensure ( :depth 1
            :host github
            :repo "precompute/cheese-capture"
            ;; :repo "~/44.2/cheese-capture"
            :build (:not elpaca--byte-compile)
            ))

(use-package imenu
  :elpaca nil
  :preface
  (add-to-list 'imenu-generic-expression
               '("Section" "^[ \t]*;;;*\\**[ \t]+\\([^\n]+\\)" 1))
  :config
  (setq imenu-max-index-time 3))

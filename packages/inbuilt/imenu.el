(use-package imenu
  :elpaca nil
  :preface
  (add-to-list 'imenu-generic-expression
               '("Section" "^[ \t]*;;;*\\**[ \t]+\\([^\n]+\\)" 1)))

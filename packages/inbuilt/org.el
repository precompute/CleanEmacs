(use-package org
  :elpaca nil
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)

;;;;; org capture
  (defvar 46-da-org-dir-path-c "/home/sys2/46/da/da.org")
  (setq org-capture-templates
        '(("p" "PDF" plain (file 46-da-org-dir-path-c)
           "** %(org-capture-pdf-c 1)
:PROPERTIES:
:PAGE: %(org-capture-pdf-c 4)
:END:
[[%(org-capture-pdf-c 2)]]
%U
#+begin_quote
%(org-capture-pdf-c 3)
#+end_quote
%?" :empty-lines 1)
          ("P" "PDF without comment" plain (file 46-da-org-dir-path-c)
           "** %(org-capture-pdf-c 1)
:PROPERTIES:
:PAGE: %(org-capture-pdf-c 4)
:END:
[[%(org-capture-pdf-c 2)]]
%U
#+begin_quote
%(org-capture-pdf-c 3)
#+end_quote" :empty-lines 1 :immediate-finish t)
          ("f" "file" plain (file 46-da-org-dir-path-c)
           "** %f
[[%F]]
%U
#+begin_quote
%i
#+end_quote
%?" :empty-lines 1)
          ("F" "file without comment" plain (file 46-da-org-dir-path-c)
           "** %f
[[%F]]
%U
#+begin_quote
%i
#+end_quote" :empty-lines 1 :immediate-finish t)
          ("c" "code" plain (file 46-da-org-dir-path-c)
           "** %f
:PROPERTIES:
:REPO_ROOT: %(org-capture-get-repository-root-c)
:END:
[[%F]]
%U
#+begin_src %(org-capture-get-major-mode-c)
%i
#+end_src
%?" :empty-lines 1)
          ("C" "code without comments" plain (file 46-da-org-dir-path-c)
           "** %f
:PROPERTIES:
:REPO_ROOT: %(org-capture-get-repository-root-c)
:END:
[[%F]]
%U
#+begin_src %(org-capture-get-major-mode-c)
%i
#+end_src" :empty-lines 1 :immediate-finish t)
          ("e" "epub" plain (file 46-da-org-dir-path-c)
           "** %(org-capture-epub-c 1)
:PROPERTIES:
:CHAPTER: %(org-capture-epub-c 4)
:CHAPTER_PROGRESS: %(org-capture-epub-c 5)
:END:
[[%(org-capture-epub-c 2)]]
%U
#+begin_quote
%(org-capture-epub-c 3)
#+end_quote
%?" :empty-lines 1)
          ("E" "epub without comments" plain (file 46-da-org-dir-path-c)
           "** %(org-capture-epub-c 1)
:PROPERTIES:
:CHAPTER: %(org-capture-epub-c 4)
:CHAPTER_PROGRESS: %(org-capture-epub-c 5)
:END:
[[%(org-capture-epub-c 2)]]
%U
#+begin_quote
%(org-capture-epub-c 3)
#+end_quote" :empty-lines 1 :immediate-finish t))))

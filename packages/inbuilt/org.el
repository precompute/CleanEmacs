(use-package org
  :elpaca nil
  :config
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-startup-indented t
        org-hide-drawer-startup t
        org-use-sub-superscripts "{}"
        org-pretty-entities t
        org-directory "~/46/org/"
        org-attach-id-dir "~/46/org/.attach/"
        org-startup-with-inline-images t
        org-fontify-done-headline t
        org-fontify-todo-headline t
        org-time-stamp-formats org-time-stamp-custom-formats
        org-tsr-regexp-both "[[<]\\([[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)[]>]\\(--?-?[[<]\\([[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)[]>]\\)?"
        org-id-locations-file "/home/sys2/46/.orgids"
        org-time-stamp-custom-formats '("<%y-%m-%d %H:%M:%S>" . "<%y-%m-%d %H:%M:%S>")
        org-support-shift-select t
        org-imenu-depth 90
        org-capture-mode-hook nil
        org-cycle-separator-lines 0
        org-todo-log-states t
        org-treat-insert-todo-heading-as-state-change t
        org-lowest-priority ?L
        org-priority-faces '((65 . error)
                             (66 . success)
                             (67 . match)
                             (68 . rainbow-delimiters-depth-1-face)
                             (69 . font-lock-variable-name-face)
                             (70 . whitespace-tab)
                             (71 . default)
                             (72 . default)
                             (73 . default)
                             (74 . default)
                             (75 . default)
                             (76 . error)))
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-overriding-header (format-time-string "%H:%M:%S"))
                        (org-super-agenda-groups
                         '((:name ""
                                  :time-grid t
                                  :transformer (org-agenda-split-string-b it " " 'font-lock-builtin-face)
                                  :log 'close
                                  :todo  ("READ" "CNCL" "DONE" "ANSWERED")
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(
                            (:name "Questions"
                                   :transformer (org-agenda-split-string it 'font-lock-doc-face)
                                   :todo "QUESTION"
                                   :order 5)
                            (:name "Comics"
                                   :transformer (org-agenda-split-string-2 it)
                                   :and (:todo ("READING") :tag "comic")
                                   :and (:todo ("TO-READ") :tag "comic")
                                   :and (:todo ("SUSPEND") :tag "comic")
                                   :discard (:and (:todo "READ" :tag "comic"))
                                   :order 12)
                            (:name "Books/Technical"
                                   :transformer (org-agenda-split-string-2-b-t it)
                                   :and (:todo ("READING") :tag "book" :tag "technical" :priority "A")
                                   :and (:todo ("READING") :tag "book" :tag "technical" :priority "B")
                                   :and (:todo ("READING") :tag "book" :tag "technical" :priority "C")
                                   :and (:todo ("READING") :tag "book" :tag "technical")
                                   :and (:todo ("REGULAR") :tag "book" :tag "technical" :priority "A")
                                   :and (:todo ("REGULAR") :tag "book" :tag "technical" :priority "B")
                                   :and (:todo ("REGULAR") :tag "book" :tag "technical" :priority "C")
                                   :and (:todo ("REGULAR") :tag "book" :tag "technical")
                                   :and (:todo ("TO-READ") :tag "book" :tag "technical" :priority "A")
                                   :and (:todo ("TO-READ") :tag "book" :tag "technical" :priority "B")
                                   :and (:todo ("TO-READ") :tag "book" :tag "technical" :priority "C")
                                   :and (:todo ("TO-READ") :tag "book" :tag "technical")
                                   :and (:todo ("SUSPEND") :tag "book" :tag "technical" :priority "A")
                                   :and (:todo ("SUSPEND") :tag "book" :tag "technical" :priority "B")
                                   :and (:todo ("SUSPEND") :tag "book" :tag "technical" :priority "C")
                                   :and (:todo ("SUSPEND") :tag "book" :tag "technical")
                                   :order 11)
                            (:name "Books"
                                   :transformer (org-agenda-split-string-2-b it) ;;nixes the default face
                                   :and (:todo ("READING" "TO-READ" "REGULAR") :tag "book" :children t)
                                   :and (:todo ("READING") :tag "book" :priority "A")
                                   :and (:todo ("READING") :tag "book" :priority "B")
                                   :and (:todo ("READING") :tag "book" :priority "C")
                                   :and (:todo ("READING") :tag "book")
                                   :and (:todo ("REGULAR") :tag "book" :priority "A")
                                   :and (:todo ("REGULAR") :tag "book" :priority "B")
                                   :and (:todo ("REGULAR") :tag "book" :priority "C")
                                   :and (:todo ("REGULAR") :tag "book")
                                   :and (:todo ("TO-READ") :tag "book" :priority "A")
                                   :and (:todo ("TO-READ") :tag "book" :priority "B")
                                   :and (:todo ("TO-READ") :tag "book" :priority "C")
                                   :and (:todo ("TO-READ") :tag "book")
                                   :and (:todo ("SUSPEND") :tag "book" :priority "A")
                                   :and (:todo ("SUSPEND") :tag "book" :priority "B")
                                   :and (:todo ("SUSPEND") :tag "book" :priority "C")
                                   :and (:todo ("SUSPEND") :tag "book")
                                   :discard (:and (:todo "READ" :tag "book"))
                                   :order 10)
                            (:name "Important"
                                   :transformer (org-agenda-split-string it 'org-agenda-date-today)
                                   :priority "A" :order 1)
                            (:name "Long Term"
                                   :transformer (org-agenda-split-string it 'font-lock-comment-face)
                                   :priority "L"
                                   :order 23)
                            (:name "Maybe..."
                                   :transformer (org-agenda-split-string it 'font-lock-variable-name-face)
                                   :todo "MYBE"
                                   :order 16)
                            (:name "Why even bother?"
                                   :transformer (org-agenda-split-string it 'default)
                                   :priority "G"
                                   :priority "H"
                                   :priority "J"
                                   :priority "K"
                                   :order 17)
                            (:name "Unimportant"
                                   :transformer (org-agenda-split-string it 'font-lock-variable-name-face)
                                   :priority<= "D"
                                   :todo "MYBE"
                                   :order 8)
                            (:name "Primary"
                                   :transformer (org-agenda-split-string it 'org-agenda-date)
                                   :and (:todo "TODO" :priority> "C")
                                   :order 2)
                            (:name "Secondary"
                                   :transformer (org-agenda-split-string it 'font-lock-type-face)
                                   :and (:todo "TODO" :priority "C")
                                   :order 3)
                            (:name "Transient"
                                   :transformer (org-agenda-split-string it 'font-lock-constant-face)
                                   :todo ("TODO" "PRTL" "INPR" "WAIT" "HOLD" "PROJ")
                                   :order 4
                                   )
                            (:discard (:todo "CNCL"))
                            (:discard (:todo "[ ]"))
                            (:discard (:todo "[-]"))
                            (:discard (:todo "[?]"))
                            (:discard (:todo "[X]"))
                            (:name "Leftovers"
                                   :transformer (org-agenda-split-string it 'font-lock-comment-face)
                                   :anything t
                                   :order 70)))))))))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "INPR(s)"
           "PRTL(h)"
           "PROJ(p)"
           "WAIT(w)"
           "|"
           "DONE(d)")
          (sequence
           "MYBE(m)"
           "|"
           "CNCL(k)")
          (sequence
           "TO-READ"
           "READING"
           "REGULAR"
           "SUSPEND"
           "|"
           "READ")
          (sequence
           "QUESTION"
           "|"
           "ANSWERED")
          (sequence
           "[ ](T)"
           "[-](S)"
           "[?](W)"
           "|"
           "[X](D)"))
        ;; org-todo-keyword-faces
         ;; '(("[-]"  . +org-todo-active)
         ;;  ("DONE" . +org-todo-done)
         ;;  ("INPR" . +org-todo-active)
         ;;  ("[?]"  . +org-todo-onhold)
         ;;  ("WAIT" . +org-todo-onhold)
         ;;  ("TO-READ" . org-todo)
         ;;  ("READING" . +org-todo-active)
         ;;  ("REGULAR" . +org-todo-onhold)
         ;;  ("SUSPEND" . +org-todo-onhold)
         ;;  ("PRTL" . +org-todo-onhold)
         ;;  ("MYBE" . +org-todo-cancel)
         ;;  ("PROJ" . +org-todo-project))
        )
;;;;; propertize
  (font-lock-add-keywords 'org-mode
                          '(("^:PROPERTIES:\n" 0 '(face nil display "∷"))
                            ("^:END:$" 0 '(face nil display "∷"))
                            ("^[[:space:]]*\\(#\\+BEGIN_SRC\\)" 1 '(face nil display "LANG"))
                            ("^[[:space:]]*\\(#\\+begin_src\\)" 1 '(face nil display "LANG"))
                            ("^[[:space:]]*\\(#\\+END.*\\)" 1 '(face nil display "END"))
                            ("^[[:space:]]*\\(#\\+end.*\\)" 1 '(face nil display "END"))
                            ("^[[:space:]]*\\(#\\+BEGIN_QUOTE\\)" 1 '(face nil display "QUOTE"))
                            ("^[[:space:]]*\\(#\\+begin_quote\\)" 1 '(face nil display "QUOTE"))
                            ("^[[:space:]]*\\(+\\) " 2 '(face font-lock-keyword-face display "⊣"))
                            ("^[[:space:]]*\\(-\\) " 2 '(face font-lock-builtin-face display "⊢"))
                            ("^[*+-]+[[:space:]]\\(\\[ \\]\\)" 1 '(face custom--org-mode-face-1 display "   "))
                            ("^[*+-]+[[:space:]]\\(\\[X\\]\\)" 1 '(face custom--org-mode-face-2 display "   "))
                            ("^[*+-]+[[:space:]]\\(\\[-\\]\\)" 1 '(face custom--org-mode-face-3 display "   "))
                            ("^[*+-]+[[:space:]]\\(\\[\\?\\]\\)" 1 '(face custom--org-mode-face-4 display "   "))
                            ("^\\(.*\\)\\(\\[\\)\\([0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)\\(]\\).*"
                             (2 '(face nil display " "))
                             (4 '(face nil display " ")))
                            ("^\\(.*\\)\\(\\[\\)\\([0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)\\(]\\).*"
                             (2 '(face nil display " "))
                             (4 '(face nil display " ")))))

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

;;;;; faces
(defface custom--org-mode-face-1
  '((t :underline nil))
  "Custom org-mode face 1")
(defface custom--org-mode-face-2
  '((t :underline nil))
  "Custom org-mode face 2")
(defface custom--org-mode-face-3
  '((t :underline nil))
  "Custom org-mode face 3")
(defface custom--org-mode-face-4
  '((t :underline nil))
  "Custom org-mode face 4")

;;;;; Functions
;;;;;; agenda
(defun org-agenda-split-string-b (str sep face)
  (interactive)
  (let* ((aaa (split-string-and-unquote str sep))
         (ca  (car aaa)))
    (concat
     (format "%s" (propertize
                   (if (string-equal ":" (substring ca -1))
                       (if (string-match-p "[[:digit:]]" (substring ca 0 1))
                           (concat "  " (substring ca 0 (- (length ca) 5))
                                   " ")
                         (concat "  " (string-pad (substring ca 0 -1) 6) " "))
                     (concat "           " ca))
                   'face face))
     (format " %s     %s %s"
             (cadr aaa)
             (cadddr aaa)
             (string-join (cddddr aaa) " ")))))

(setq org-agenda-ss-seq-3 0)
(defun org-agenda-split-string-2 (str)
  (interactive)
  (if (or (eq nil org-agenda-ss-seq)
          (= 0 org-agenda-ss-seq))
      (setq org-agenda-ss-seq 0))
  (setq org-agenda-ss-seq (+ 1 org-agenda-ss-seq))
  (if (> org-agenda-ss-seq-3 0)
      (setq org-agenda-ss-seq-3 (- org-agenda-ss-seq-3 1)))
  (let* ((str  (replace-regexp-in-string "comic:" "" str))
         (str  (replace-regexp-in-string " :$" "" str))
         (aa   (split-string-and-unquote str))
         (ba   (butlast aa))
         (aal  (car (last aa)))
         (bal  (car (last ba))))
    (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" aal)
        (setq org-agenda-ss-seq-3
              (+ 1
                 (- (string-to-number
                     (cadr (split-string-and-unquote (substring aal 1 -1) "/")))
                    (string-to-number
                     (car (split-string-and-unquote (substring aal 1 -1) "/"))))))
      (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" bal)
          (setq org-agenda-ss-seq-3
                (+ 1
                   (- (string-to-number
                       (cadr (split-string-and-unquote (substring bal 1 -1) "/")))
                      (string-to-number
                       (car (split-string-and-unquote (substring bal 1 -1) "/")))))))))
  (let* ((aa   (split-string-and-unquote str))
         (ba   (butlast aa))
         (aal  (car (last aa)))
         (bal  (car (last ba)))
         (sm1  (string-match "\[[[:digit:]]*/[[:digit:]]*\]" aal))
         (sm2  (string-match "\[[[:digit:]]*/[[:digit:]]*\]" bal)))
    (concat
     (format "   %s    %s%s %s %s"
             (propertize (string-pad (number-to-string org-agenda-ss-seq) 4 ?0 t)
                         'face 'font-lock-builtin-face)
             (car (butlast (butlast (cdr aa))))
             (if (and (> org-agenda-ss-seq-3 0)
                      (not (or sm1 sm2)))
                 ;; (propertize " ⇶"
                 (propertize " ::"
                             'face 'highlight-quoted-symbol)
               "")
             (if (or sm1 sm2)
                 (propertize
                  (mapconcat #'concat (cdr (butlast (butlast (cdr aa)))) " ")
                  'face '(bold underline))
               (mapconcat #'concat (cdr (butlast (butlast (cdr aa)))) " "))
             (string-trim
              (if sm1
                  (propertize aal 'face 'highlight-quoted-symbol)
                (if sm2
                    (concat (propertize bal 'face 'highlight-quoted-symbol) " " aal)
                  (concat bal " " aal))))))))

(setq org-agenda-ss-seq-0 0)
(defun org-agenda-split-string (str face)
  (interactive)
  (if (> org-agenda-ss-seq-0 0)
      (setq org-agenda-ss-seq-0 (- org-agenda-ss-seq-0 1)))
  (let* ((aa   (split-string-and-unquote str))
         (ba   (butlast aa))
         (aal  (car (last aa)))
         (bal  (car (last ba))))
    (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" aal)
        (setq org-agenda-ss-seq-0
              (+ 1
                 (- (string-to-number
                     (cadr (split-string-and-unquote (substring aal 1 -1) "/")))
                    (string-to-number
                     (car (split-string-and-unquote (substring aal 1 -1) "/"))))))
      (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" bal)
          (setq org-agenda-ss-seq-0
                (+ 1
                   (- (string-to-number
                       (cadr (split-string-and-unquote (substring bal 1 -1) "/")))
                      (string-to-number
                       (car (split-string-and-unquote (substring bal 1 -1) "/")))))))))
  (let* ((aa  (split-string-and-unquote str))
         (aaa (substring (car aa) 0 6))
         (ba  (butlast aa))
         (aal (car (last aa)))
         (bal (car (last ba)))
         (sm1 (string-match "\[[[:digit:]]*/[[:digit:]]*\]" aal))
         (sm2 (string-match "\[[[:digit:]]*/[[:digit:]]*\]" bal)))
    (concat
     (format "%s %s %s%s %s %s"
             (propertize aaa 'face face)
             (propertize
              (string-pad
               (substring
                (shell-command-to-string
                 (concat "dateutils.ddiff -i '%y%m%d' -f '%d' " aaa " now "))
                0 -1)
               3 nil t)
              'face 'font-lock-builtin-face)
             (car (butlast (butlast (cdr aa))))
             (if (and (> org-agenda-ss-seq-0 0)
                      (not (or sm1 sm2)))
                 (propertize " ::"
                             'face 'highlight-quoted-symbol)
               "")
             (if (or sm1 sm2)
                 (propertize
                  (mapconcat #'concat (cdr (butlast (butlast (cdr aa)))) " ")
                  'face '(bold underline))
               (mapconcat #'concat (cdr (butlast (butlast (cdr aa)))) " "))
             (string-trim
              (if sm1
                  (propertize aal 'face 'highlight-quoted-symbol)
                (if sm2
                    (concat (propertize bal 'face 'highlight-quoted-symbol) " " aal)
                  (concat bal " " aal))))))))

(defvar org-agenda-ss-seq-2 nil)
(defun org-agenda-split-string-2-b (str)
  (interactive)
  (if (or (eq nil org-agenda-ss-seq-2)
          (= 0 org-agenda-ss-seq-2))
      (setq org-agenda-ss-seq-2 0))
  (setq org-agenda-ss-seq-2 (+ 1 org-agenda-ss-seq-2))
  (let* ((aa   (split-string-and-unquote str))
         (ba   (butlast aa))
         (aal  (car (last aa)))
         (bal  (car (last ba)))
         (ccc  (butlast (butlast (cddr aa))))
         (cc   (mapconcat #'concat ccc " "))
         (ccs  (split-string-and-unquote cc "|"))
         (dd   (cadr aa))
         (cccs (car (cddr aa)))
         (ccsc (split-string-and-unquote (car ccs)))
         (stag (car ccsc))
         (auth (string-join (cdr ccsc) " ")))
    (concat
     (format "   %s    %s %s %s %s"
             (propertize (string-pad (number-to-string org-agenda-ss-seq-2) 4 ?0 t)
                         'face 'font-lock-builtin-face)
             (if (string-equal dd "READ")
                 (propertize dd 'face 'org-done)
               (if (string-equal dd "TO-READ")
                   (propertize dd 'face 'org-todo)
                 dd))
             (if (string-match org-priority-regexp stag)
                 (concat stag " "
                         (string-pad
                          (propertize
                           (string-trim auth)
                           'face '(underline bold))
                          20))
               (concat "     "
                       (string-pad
                        (propertize
                         (string-trim (concat stag " " auth))
                         'face '(underline bold))
                        20)))
             (mapconcat #'concat (cdr ccs) " ")
             (string-trim
              (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" aal)
                  (propertize aal 'face 'highlight-quoted-symbol)
                (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" bal)
                    (concat (propertize bal 'face 'highlight-quoted-symbol) " " aal)
                  (concat bal " " aal))))))))

(defvar org-agenda-ss-seq-3 nil)
(defun org-agenda-split-string-2-b-t (str)
  (interactive)
  (if (or (eq nil org-agenda-ss-seq-3)
          (= 0 org-agenda-ss-seq-3))
      (setq org-agenda-ss-seq-3 0))
  (setq org-agenda-ss-seq-3 (+ 1 org-agenda-ss-seq-3))
  (let* ((str  (replace-regexp-in-string "book:" "" str))
         (str  (replace-regexp-in-string "technical:" "" str))
         (str  (replace-regexp-in-string " :$" "" str))
         (aa   (split-string-and-unquote str))
         (ba   (butlast aa))
         (aal  (car (last aa)))
         (bal  (car (last ba)))
         (ccc  (butlast (butlast (cddr aa))))
         (cc   (mapconcat #'concat ccc " "))
         (ccs  (split-string-and-unquote cc "|"))
         (dd   (cadr aa))
         (cccs (car (cddr aa)))
         (ccsc (split-string-and-unquote (car ccs)))
         (stag (car ccsc))
         (auth (string-join (cdr ccsc) " ")))
    (concat
     (format "   %s    %s %s %s %s"
             (propertize (string-pad (number-to-string org-agenda-ss-seq-3) 4 ?0 t)
                         'face 'font-lock-builtin-face)
             (if (string-equal dd "READ")
                 (propertize dd 'face 'org-done)
               (if (string-equal dd "TO-READ")
                   (propertize dd 'face 'org-todo)
                 dd))
             (if (string-match org-priority-regexp stag)
                 (concat stag " "
                         (string-pad
                          (propertize
                           (string-trim auth)
                           'face '(underline bold))
                          20))
               (concat "     "
                       (string-pad
                        (propertize
                         (string-trim (concat stag " " auth))
                         'face '(underline bold))
                        20)))
             (mapconcat #'concat (cdr ccs) " ")
             (string-trim
              (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" aal)
                  (propertize aal 'face 'highlight-quoted-symbol)
                (if (string-match "\[[[:digit:]]*/[[:digit:]]*\]" bal)
                    (concat (propertize bal 'face 'highlight-quoted-symbol) " " aal)
                  (concat bal " " aal))))))))

;;;;;; other
(defun org-agenda-refresh-c ()
  (interactive)
  (progn
    (let ((filelist (append
                     (reverse
                      (directory-files-recursively
                       "~/46/da/timelog" (rx ".org" eos)))
                     (directory-files-recursively
                      "~/46/da/read" (rx ".org" eos)))))
      (setq org-agenda-files nil)
      (setq org-agenda-files filelist)
      (message "Agenda refreshed"))))

(defun org-agenda-to-file-c ()
  (interactive)
  (save-excursion
    (org-agenda-write
     (concat
      (file-name-as-directory "~/46/agenda/")
      (format-time-string "%y%m%d.%H%M%S.agenda"))
     nil nil "*Org Agenda*")))

;;;;;; org-dwim
(defun org-ret-dwim ()
  "Do whatever is possible for current point.
Copied from +org/dwim-at-point in Doom."
  (interactive)
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

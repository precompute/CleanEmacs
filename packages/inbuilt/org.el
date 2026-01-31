(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
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
        org-ellipsis nil
        org-tsr-regexp-both "[[<]\\([[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)[]>]\\(--?-?[[<]\\([[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)[]>]\\)?"
        org-id-locations-file (expand-file-name "~/46/.orgids")
        org-time-stamp-custom-formats '("<%y-%m-%d %H:%M:%S>" . "<%y-%m-%d %H:%M:%S>")
        org-time-stamp-formats org-time-stamp-custom-formats
        org-support-shift-select t
        org-imenu-depth 90
        org-return-follows-link t
        org-capture-mode-hook nil
        org-cycle-separator-lines 0
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-todo-log-states t
        org-log-done 'time
        org-tags-column 0
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
;;;;; propertize
  (font-lock-add-keywords 'org-mode
                          '(("^:PROPERTIES:\n" 0 '(face nil display "::"))
                            ("^:END:$" 0 '(face nil display "::"))
                            ;; ("^[[:space:]]*\\(#\\+BEGIN_SRC\\)" 1 '(face nil display "LANG"))
                            ;; ("^[[:space:]]*\\(#\\+begin_src\\)" 1 '(face nil display "LANG"))
                            ;; ("^[[:space:]]*\\(#\\+END.*\\)" 1 '(face nil display "END"))
                            ;; ("^[[:space:]]*\\(#\\+end.*\\)" 1 '(face nil display "END"))
                            ;; ("^[[:space:]]*\\(#\\+BEGIN_QUOTE\\)" 1 '(face nil display "QUOTE"))
                            ;; ("^[[:space:]]*\\(#\\+begin_quote\\)" 1 '(face nil display "QUOTE"))
                            ("^[[:space:]]*\\(+\\) " 1 '(face font-lock-keyword-face display "⊣"))
                            ("^[[:space:]]*\\(-\\) " 1 '(face font-lock-builtin-face display "⊢"))
                            ("^[*+-]+[[:space:]]\\(\\[ \\]\\)" 1 '(face custom--org-mode-face-1 display "   "))
                            ("^[*+-]+[[:space:]]\\(\\[X\\]\\)" 1 '(face custom--org-mode-face-2 display "   "))
                            ("^[*+-]+[[:space:]]\\(\\[-\\]\\)" 1 '(face custom--org-mode-face-3 display "   "))
                            ("^[*+-]+[[:space:]]\\(\\[\\?\\]\\)" 1 '(face custom--org-mode-face-4 display "   "))
                            ;; ("^\\(.*\\)\\(\\[\\)\\([0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)\\(]\\).*"
                            ;;  (2 '(face nil display " "))
                            ;;  (4 '(face nil display " ")))
                            ;; ("^\\(.*\\)\\(\\[\\)\\([0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)\\(]\\).*"
                            ;;  (2 '(face nil display " "))
                            ;;  (4 '(face nil display " ")))
                            ))

;;;;; org capture
;;;;;; org-capture-* functions
  (defun org-capture-pdf-c (action)
    "Capture the active region of the pdf-view buffer."
    (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
           (pdf-buf (get-buffer pdf-buf-name)))
      (if (buffer-live-p pdf-buf)
          (cond
           ((= action 1)
            (with-current-buffer pdf-buf
              (buffer-name)))
           ((= action 2)
            (with-current-buffer pdf-buf
              (buffer-file-name)))
           ((= action 3)
            (with-current-buffer pdf-buf
              (if (pdf-view-active-region-p)
                  (car (pdf-view-active-region-text))
                (ignore-errors
                  (buffer-substring-no-properties (region-beginning) (region-end))))))
           ((= action 4)
            (with-current-buffer pdf-buf
              (number-to-string (pdf-view-current-page)))))
        (user-error "Buffer %S not alive." pdf-buf-name))))

  (defun org-capture-epub-c (action)
    "Capture the active region of the nov (epub) buffer."
    (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
           (pdf-buf (get-buffer pdf-buf-name)))
      (if (buffer-live-p pdf-buf)
          (cond
           ((= action 1)
            (with-current-buffer pdf-buf
              (buffer-name)))
           ((= action 2)
            (with-current-buffer pdf-buf
              nov-file-name))
           ((= action 3)
            (with-current-buffer pdf-buf
              (ignore-errors
                (buffer-substring-no-properties (region-beginning) (region-end)))))
           ((= action 4)
            (with-current-buffer pdf-buf
              (save-excursion
                (goto-char (point-min))
                (buffer-substring-no-properties
                 (point-min) (point-at-eol)))))
           ((= action 5)
            (with-current-buffer pdf-buf
              (save-excursion
                (let ((twords (count-words (point-min) (point-max)))
                      (cpwords (count-words (point-min) (region-end))))
                  (format "(%s/%s) %s%%"
                          nov-documents-index (length nov-documents)
                          (/ (* 100 cpwords) twords))))))))))

  (defun org-capture-get-major-mode-c ()
    "Get the major-mode of the buffer Capture was called from."
    (let* ((c-buf-name (plist-get org-capture-plist :original-buffer))
           (c-buf (get-buffer c-buf-name)))
      (if (buffer-live-p c-buf)
          (with-current-buffer c-buf
            (substring
             (symbol-name major-mode)
             0 -5)))))

  (defun org-capture-get-repository-root-c ()
    "Get the root of the repository Capture was called from."
    (let* ((c-buf-name (plist-get org-capture-plist :original-buffer))
           (c-buf (get-buffer c-buf-name)))
      (if (buffer-live-p c-buf)
          (with-current-buffer c-buf
            (locate-dominating-file "." ".git")))))

  (defun org-capture-notmuch-c (valtype &optional org-prop)
    "Get value VALTYPE from the current notmuch-show buffer.
When ORG-PROP is t, add appropriate property drawer prefixes."
    (let* ((c-buf-name (plist-get org-capture-plist :original-buffer))
           (c-buf (get-buffer c-buf-name))
           (valtype (substring (symbol-name valtype) 1)))
      (when (buffer-live-p c-buf)
        (with-current-buffer c-buf
          (let* ((r (funcall (intern (concat "notmuch-show-get-" valtype))))
                 (r (if (string-equal "to" valtype)
                        (if r r (notmuch-show-get-header :Delivered-To))
                      r)))
            (when r
              (if org-prop
                  (format "\n:MAIL-%s: %s" (upcase valtype) r)
                (format "%s" r))))))))

  (defun org-capture-generic-c ()
    "Return the heading for a generic capture."
    (let* ((c-buf-name (plist-get org-capture-plist :original-buffer))
           (c-buf (get-buffer c-buf-name))
           (m-mode (with-current-buffer c-buf (symbol-name major-mode))))
      (format "(%s) - %s" m-mode c-buf)))

  (defvar org-dir-path-c (expand-file-name "~/46/da/da.org"))
  (setq org-capture-templates
        '(("g" "Generic" plain (file org-dir-path-c)
           "** %(org-capture-generic-c)
[[%F]]
%U
#+begin_quote
%i
#+end_quote
%?" :empty-lines 1)
          ("G" "Generic without comment" plain (file org-dir-path-c)
           "** %(org-capture-generic-c)
[[%F]]
%U
#+begin_quote
%i
#+end_quote
%?" :empty-lines 1 :immediate-finish t)
          ("p" "PDF" plain (file org-dir-path-c)
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
          ("P" "PDF without comment" plain (file org-dir-path-c)
           "** %(org-capture-pdf-c 1)
:PROPERTIES:
:PAGE: %(org-capture-pdf-c 4)
:END:
[[%(org-capture-pdf-c 2)]]
%U
#+begin_quote
%(org-capture-pdf-c 3)
#+end_quote" :empty-lines 1 :immediate-finish t)
          ("f" "file" plain (file org-dir-path-c)
           "** %f
[[%F]]
%U
#+begin_quote
%i
#+end_quote
%?" :empty-lines 1)
          ("F" "file without comment" plain (file org-dir-path-c)
           "** %f
[[%F]]
%U
#+begin_quote
%i
#+end_quote" :empty-lines 1 :immediate-finish t)
          ("c" "code" plain (file org-dir-path-c)
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
          ("C" "code without comments" plain (file org-dir-path-c)
           "** %f
:PROPERTIES:
:REPO_ROOT: %(org-capture-get-repository-root-c)
:END:
[[%F]]
%U
#+begin_src %(org-capture-get-major-mode-c)
%i
#+end_src" :empty-lines 1 :immediate-finish t)
          ("m" "mail" plain (file org-dir-path-c)
           "** MAIL from \"%(org-capture-notmuch-c :from)\" [%(org-capture-notmuch-c :timestamp)] : %(org-capture-notmuch-c :subject)
:PROPERTIES:%(org-capture-notmuch-c :from t)%(org-capture-notmuch-c :to t)%(org-capture-notmuch-c :cc t)%(org-capture-notmuch-c :filename t)%(org-capture-notmuch-c :date t)
:END:
[[%(org-capture-notmuch-c :filename)]]
%U
#+begin_quote
%i
#+end_quote
%?" :empty-lines 1)
          ("M" "mail without comments" plain (file org-dir-path-c)
           "** MAIL from \"%(org-capture-notmuch-c :from)\" [%(org-capture-notmuch-c :timestamp)] : %(org-capture-notmuch-c :subject)
:PROPERTIES:%(org-capture-notmuch-c :from t)%(org-capture-notmuch-c :to t)%(org-capture-notmuch-c :cc t)%(org-capture-notmuch-c :filename t)%(org-capture-notmuch-c :date t)
:END:
[[%(org-capture-notmuch-c :filename)]]
%U
#+begin_quote
%i
#+end_quote" :empty-lines 1 :immediate-finish t)
          ( "e" "epub" plain (file org-dir-path-c)
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
          ("E" "epub without comments" plain (file org-dir-path-c)
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
  "Custom org-mode face 1.")
(defface custom--org-mode-face-2
  '((t :underline nil))
  "Custom org-mode face 2.")
(defface custom--org-mode-face-3
  '((t :underline nil))
  "Custom org-mode face 3.")
(defface custom--org-mode-face-4
  '((t :underline nil))
  "Custom org-mode face 4.")
(defface custom--org-todo-active
  '((t (:inherit (bold font-lock-constant-face org-todo fixed-pitch))))
  "Custom org-mode todo-active face.")
(defface custom--org-todo-done
  '((t (:inherit (bold success org-todo fixed-pitch))))
  "Custom org-mode todo-done face.")
(defface custom--org-todo-onhold
  '((t (:inherit (bold warning org-todo fixed-pitch))))
  "Custom org-mode todo-onhold face.")
(defface custom--org-todo-cancel
  '((t (:inherit (bold font-lock-builtin-face org-todo fixed-pitch))))
  "Custom org-mode todo-cancel face.")
(defface custom--org-todo-project
  '((t (:inherit (bold font-lock-doc-face org-todo fixed-pitch))))
  "Custom org-mode todo-project face.")

(defun set-custom-org-faces-c (&optional theme)
  "Set custom org faces for THEME."
  (with-eval-after-load 'org-indent
    (set-face-attribute 'org-indent nil :foreground (face-attribute 'default :background))))
(add-to-list 'enable-theme-functions 'set-custom-org-faces-c)

;;;;; Functions
;;;;;; org-dwim
(defun org-dwim-c ()
  "Do the best possible thing for POINT.
- Toggle checkbox if available."
  (interactive)
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        (`headline
         (cond ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done) 'todo 'done))))
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics))
        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16))))
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics))))))

;;;;; images in org-mode
;; (defun dynamic-image-size-org-mode ()
;;   (interactive)
;;   (if (eq major-mode 'org-mode)
;;       (if olivetti-mode
;;           (setq-local org-image-actual-width ;; (* (window-pixel-width)
;;                       ;;    (/ (float olivetti-body-width)
;;                       ;;       (float (window-width))))
;;                       olivetti-body-width))
;;     (setq-local org-image-actual-width (* (/ 3.0 4.0) (window-pixel-width))))
;;   (if (eq major-mode 'org-mode)
;;       (org-redisplay-inline-images)))
;; (add-to-list 'window-size-change-functions 'dynamic-image-size-org-mode)

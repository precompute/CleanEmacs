;;; Functions
;;;; timestamp
(defun timestamp ()
  "Insert string for the current time formatted like '2:34 PM' or 1507121460 https://emacs.stackexchange.com/questions/7250/in-org-mode-how-to-insert-timestamp-with-todays-date"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "[%02y-%02m-%02d %02H:%02M:%02S] ")) ;;space at end
  )

(defun timestamp-no-time ()
  "timestamp without time"
  (interactive)
  (insert (format-time-string "[%02y-%02m-%02d]")) ;;no space at end
  )

;;;; convenience
(defun consult-ripgrep-local ()
  (interactive)
  (let ((consult-project-function nil))
    (consult-ripgrep)))

(defun toggle-eldoc-box ()
  (interactive)
  (if (boundp 'toggle-eldoc-box--history)
      (progn
        (if (eq 1 (car toggle-eldoc-box--history))
            (funcall-interactively #'eldoc-box-hover-mode)
          (if (eq 1 (cdr toggle-eldoc-box--history))
              (funcall-interactively #'eldoc-box-hover-at-point-mode))))
    (defvar-local toggle-eldoc-box--history '(0 0))
    (let ((msg ""))
      (if eldoc-box-hover-mode
          (progn
            (setq-local toggle-eldoc-box--history
                        (list 1 (cdr toggle-eldoc-box--history)))
            (concat msg " `eldoc-box-hover-mode' disabled.")))
      (if eldoc-box-hover-at-point-mode
          (progn
            (setq-local toggle-eldoc-box--history
                        (list (car toggle-eldoc-box--history) 1))
            (concat msg " `eldoc-box-hover-at-point-mode' disabled.")))
      (message msg))))

;;;; completion
;;;;; kill-new from global paste
(defun kill-new-from-global-paste-c ()
  (interactive)
  (require 'sqlite3)
  (let ((mylist '()))
    (let* ((dbh (sqlite3-open "/home/sys2/.local/share/zeitgeist/activity.sqlite" sqlite-open-readonly))
           (stmt (sqlite3-prepare dbh "select * from text order by id desc limit 1000")))
      (while (= sqlite-row (sqlite3-step stmt))
        (cl-destructuring-bind (id text) (sqlite3-fetch stmt)
          (setq mylist (nconc mylist (list text)))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh))
    (kill-new
     (let* ((vertico-sort-function nil)
            (rlist (completing-read-multiple " " mylist))
            (rstr ""))
       (while (length> rlist 0)
         (setq rstr (concat rstr (pop rlist) "\n")))
       rstr))))

;;;; unicode
(defun insert-char-5-discard-end ()
  "Read 5 keyboard inputs, interpret first four as a hexadecimal number, discard the fifth and insert the remaining as a character.
https://emacs.stackexchange.com/questions/74526/programmatically-invoking-insert-char-for-compatibility-with-unicode-insertion-i/74527#74527"
  (interactive)
  (let* ((k1 (read-key-sequence "_____"))
         (k2 (read-key-sequence (concat k1 "____")))
         (k3 (read-key-sequence (concat k1 k2 "___")))
         (k4 (read-key-sequence (concat k1 k2 k3 "__")))
         (k5 (read-key (concat k1 k2 k3 k4 "_")))
         (charcode (cl-parse-integer (concat k1 k2 k3 k4) :radix 16)))
    (insert-char charcode)
    (message (concat k1 k2 k3 k4 " => " (char-to-string charcode)))))

;;;; screenshot
(defun screenshot-png-c ()
  "Save a screenshot of the current frame as an PNG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".png"))
         (data (x-export-frames nil 'png)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;;;; git
;;;;; auto-commit
(defun git-auto-time-commit ()
  "adds everything and commits with the date as the commit message."
  (interactive)
  (let* ((rootdir (expand-file-name (vc-root-dir)))
         (committime (substring (shell-command-to-string "date +%Y-%m-%d-%H:%M:%S") 0 -1))
         (gitadd (concat "git -C \"" rootdir "\" add --all")) ;;removed --quiet because it wasn't working, and the output is unobstrusive anyway.
         (gitcommit (concat "git -C \"" rootdir "\" commit -m \"" committime "\"")))
    (progn
      (message gitadd)
      (message gitcommit)
      (shell-command gitadd)
      (shell-command gitcommit))))

(defun git-prompt-commit () ;;it just works!!!!
  "adds everything and commits with the commit message provided by the user."
  (interactive)
  (let* ((rootdir (expand-file-name (vc-root-dir)))
         (commitmsg (read-string "Commit Message: "))
         (gitadd (concat "git -C \"" rootdir "\" add --all"))
         (gitcommit (concat "git -C \"" rootdir "\" commit -m \"" commitmsg "\"")))
    (progn
      (message gitadd)
      (message gitcommit)
      (shell-command gitadd)
      (shell-command gitcommit))))

(defun locate-git-file-c ()
  "locate the nearest .git file upwards"
  (interactive)
  (find-file
   (locate-dominating-file "." ".git"))
  (dired-jump nil ".git")
  (message "Viewing folder with vc root!"))

;;;; Shell Command Calls
;;;;; textmind stats
(defun textmind-stats-call ()
  (interactive)
  (async-shell-command "/home/sys2/44.1/python/textmindstats/textmindstats"))

;;;;; shell
(defun open-shell-split-c ()
  "Split window and open shell"
  (interactive)
  (evil-window-split)
  (eshell))

(defun open-shell-vsplit-c ()
  "Vsplit window and open shell"
  (interactive)
  (evil-window-vsplit)
  (eshell))

(defun open-external-term-here-c ()
  "Open `ter' in the current directory."
  (interactive)
  (shell-command (concat "xfce4-terminal --working-directory=" default-directory)))

;;;; Window Functions
;; It's like golden-ratio but just one function.
;; width, then height.
(defun make-window-larger-c (&optional ratio)
  "Makes the current window larger or smaller.
RATIO is the ratio used, default is 0.618.
It switches the width before the height."
  (interactive)
  (save-excursion
    (let* ((postv      (if olivetti-mode
                           (progn (olivetti-mode -1) t)
                         nil))
           (wwidth     (window-width))
           (wheight    (window-height))
           (fwidth     (frame-width))
           (fheight    (frame-height))
           (ratio      (if ratio ratio 0.618))
           (fl/fheight (floor (* ratio fheight)))
           (fl/fwidth  (floor (* ratio fwidth))))
      (if (window-full-width-p)
          (if (window-full-height-p) (message "Nothing to resize.")
            (if (>= wheight fl/fheight)
                (if (>= wwidth fl/fwidth)
                    (message "Nothing to resize.")
                  (enlarge-window (- fl/fwidth wwidth) t))
              (enlarge-window (- fl/fheight wheight))))
        (if (>= wwidth fl/fwidth)
            (if (>= wheight fl/fheight)
                (message "Nothing to resize.")
              (enlarge-window (- fl/fheight wheight)))
          (enlarge-window (- fl/fwidth wwidth) t)))
      (if postv (olivetti-mode 1)))))

(defun split-enlarge-follow-mode-toggle nil
  "Split window, enlarge left and then enable follow-mode."
  (interactive)
  (progn
    (call-interactively #'evil-window-vsplit)
    (call-interactively #'evil-window-left)
    (call-interactively #'follow-mode 1)))

;;;; org-*
;;;;; org-id-*
;;org id update recursive function, from discord
(defun org-id-update-recursively-c ()
  "Get all files in `org-directory' recursively and update org IDs."
  (interactive)
  (org-id-update-id-locations
   (append
    (directory-files-recursively org-directory (rx ".org" eos))
    (directory-files-recursively "~/46/da/timelog" (rx ".org" eos))
    (directory-files-recursively "~/46/da/read" (rx ".org" eos))
    (directory-files-recursively "~/46/textmind" (rx ".org" eos)))))

(defun org-id-update-46ca-recursively-c ()
  "Get all files in 46/ca recursively and update org IDs."
  (interactive)
  (let ((org-id-locations-file "/home/sys1/46/ca/.orgids")
        (recentf-used-hooks nil))
    (org-id-update-id-locations
     (directory-files-recursively "/home/sys1/46/ca";; (file-name-directory buffer-file-name)
                                  ".org"))))

;;;;; org-timelogrefile-*
(defun org-timelogrefile-base-c (givendate)
  (save-restriction
    (org-narrow-to-subtree)
    (write-region
     (concat "#+TITLE: " givendate "\n\n")
     nil (concat "/home/sys2/46/da/time/" givendate ".org"))
    (write-region
     (point-min) (point-max)
     (concat "/home/sys2/46/da/time/" givendate ".org") t)))

(defun org-timelogrefile-interactive-c (givendate)
  (interactive "sDate:")
  (org-timelogrefile-base-c givendate)
  )

(defun org-timelogrefile-c ()
  (interactive)
  (back-to-indentation)
  (let ((givendate (concat
                    (buffer-substring-no-properties (+ (point) 3) (+ (point) 5))
                    (buffer-substring-no-properties (+ (point) 6) (+ (point) 8))
                    (buffer-substring-no-properties (+ (point) 9) (+ (point) 11))
                    )))
    (org-timelogrefile-base-c givendate)))

;;;;; org-capture-*
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
;;;; transient
;;;;; org-agenda transient
(transient-define-prefix org-agenda-transient ()
  "Transient for org-agenda"
  :transient-suffix     'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Org Agenda"
   ;; [("m" "ShiftLeft" org-shiftleft) ;;useless
   ;;  ("i" "ShiftRight" org-shiftright)
   ;;  ("n" "ShiftDown" org-shiftdown)
   ;;  ("e" "ShiftUp" org-shiftup)]
   [("k" "Agenda Priority Down" org-agenda-priority-down)
    ("j" "Agenda Priority Up" org-agenda-priority-up)]
   [("t" "Agenda TODO" org-agenda-todo)
    ("T" "Agenda set tags" org-agenda-set-tags)
    ("d" "Agenda Deadline" org-agenda-deadline)
    ("s" "Agenda Schedule" org-agenda-schedule)]
   [("r" "Agenda ReDo All" org-agenda-redo-all)
    ("u" "Agenda UnDo" org-agenda-undo)]
   [("<tab>" "Agenda Goto" org-agenda-goto :transient nil)
    ("w" "Agenda to File" org-agenda-to-file-c)]]
  ["Movement"
   [("<down>" "Down" next-line)]
   [("<up>" "Up" previous-line)]])

;;;;; toggle-modes-transient-c
(transient-define-prefix toggle-modes-transient-c ()
  :transient-suffix     'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Minor Modes"
   [("oo" (lambda () (toggle-modes-transient--description 'olivetti-mode "Olivetti "))
     olivetti-mode)
    ;; ("OO" "'fancy local" olivetti-fancy-local-c :transient nil)
    ;; ("Oo" "'fancy global" olivetti-fancy-c :transient nil)
    ]
   [("t" (lambda () (toggle-modes-transient--description 'truncate-lines "Truncate Lines"))
     toggle-truncate-lines)
    ;; ("f" (lambda () (toggle-modes-transient--description 'flycheck-mode "Flycheck"))
    ;;  flycheck-mode)
    ("i" (lambda () (toggle-modes-transient--description 'highlight-indent-guides-mode "Indent Guides"))
     highlight-indent-guides-mode)]
   [;; ("l" (lambda () (toggle-modes-transient--description 'display-line-numbers "Line Numbers"))
    ;;  doom/toggle-line-numbers)
    ("v" (lambda () (toggle-modes-transient--description 'visual-line-mode "Visual Lines"))
     visual-line-mode)
    ("m" (lambda () (toggle-modes-transient--description 'mixed-pitch--applied-p "Mixed Pitch"))
     mixed-pitch-mode)]
   [("L" (lambda () (toggle-modes-transient--description 'hl-line-mode "Highlight Lines"))
     hl-line-mode)
    ]
   ]
  ["Major Modes"
   [("0" "Clean Mode" clean-mode)]
   [("1" "Fundamental Mode" fundamental-mode)]]
  ["Other"
   [("B" "Revert Buffer" revert-buffer)
    ;; ("!" (lambda () (toggle-modes-transient--description 'custom-transparency "Transparency"))
    ;;  toggle-transparency-c)
    ;; ("z" "*scratch*" scratch-pop)
    ]
   [;; ("@" "Ref. Proj. cache" projectile-invalidate-cache)
    ("#" "Open Terminal here" open-external-term-here-c)
    ("F" "Find File" find-file)]
   [("$$" "Highlight regexp" highlight-regexp :transient nil) ;;buffer operations DO NOT WORK
    ("%%" "Highlight phrase" highlight-phrase :transient nil)
    ("$%" "Unhighlight regexp/phrase" unhighlight-regexp :transient nil)]
   [("<f9>" "Save Buffer" save-buffer)
    ;; (";" "Sudo file" doom/sudo-this-file)
    ("T" "Switch theme" load-theme)]
   ]
  ;; ["+"
  ;;  [("+" "New Id" (lambda () (interactive) (find-file "~/46/da/id")) :transient nil)
  ;;   ("<return>" "New TimeLog" new-timelog-c :transient nil)]
  ;;  [("-" "Open da.org" (lambda () (interactive) (find-file "~/46/da/da.org")) :transient nil)]
  ;;  [("r" "Refresh Agenda" (lambda () (interactive)
  ;;                           (progn
  ;;                             (let ((filelist (directory-files-recursively
  ;;                                              "~/46/da/timelog" (rx ".org" eos))))
  ;;                               (setq org-agenda-files nil)
  ;;                               (setq org-agenda-files filelist)
  ;;                               (message "Agenda refreshed")))))
  ;;   ("." "New TimeLog Log" org-todo-today-timestamp-c :transient nil)]
  ;;  [("/" "New TimeLog TODO" org-todo-today-timestamp-todo-c :transient nil)
  ;;   ("a" "Agenda+Todo" (lambda () (interactive) (org-agenda nil "o")
  ;;                        ) :transient nil)]
  ;;  ]
  )

;;;;;;; description function
(defun toggle-modes-transient--description (var str)
  (interactive)
  (format "%s" (concat (if (buffer-local-value var (current-buffer))
                           (propertize "ON " 'face '(bold success))
                         (propertize "OFF " 'face '(bold error)))
                       str)))

;;;;; batch main log refile
(transient-define-prefix main-log-refile-transient ()
  "A transient for inserting Level 1 date headings at appropriate points."
  :transient-suffix     'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Date"
   [("q" (lambda () (interactive)
           (unless (boundp 'main-log-init-date)
             (setq main-log-init-date (format-time-string
                                       "%y-%m-%d")))
           (concat "Current Date: "
                   (propertize main-log-init-date
                               'face 'font-lock-keyword-face)))
     (lambda () (interactive)
       (let ((datevar (read-string "Init Date: ")))
         (setq main-log-init-date datevar))))
    ("b" "Insert L1 Above Current"
     (lambda () (interactive)
       (save-excursion
         (evil-open-above 0)
         (insert
          (concat "* ["
                  main-log-init-date
                  "]")))))]
   [("i" "Increment Day"
     (lambda () (interactive)
       (let ((newlast (+ 1 (string-to-number
                            (substring main-log-init-date 6 8)))))
         (setq main-log-init-date
               (concat (substring main-log-init-date 0 6)
                       (if (= (length (number-to-string newlast)) 1)
                           "0")
                       (number-to-string newlast))))))
    ("I" "Increment Month"
     (lambda () (interactive)
       (let ((newmid (+ 1 (string-to-number
                           (substring main-log-init-date 3 5)))))
         (setq main-log-init-date
               (concat (substring main-log-init-date 0 2) "-"
                       (if (= (length (number-to-string newmid)) 1)
                           "0")
                       (number-to-string newmid) "-"
                       (substring main-log-init-date 6 8))))))]
   ]
  ["Movement"
   [("n" "Next Heading" outline-next-heading)
    ("e" "Prev Heading" outline-previous-heading)]
   [("j" "Next Line" next-line)
    ("k" "Prev Line" previous-line)]
   [("h" "previous level" org-up-element)
    ("l" "next level" org-down-element)]
   [("<down>" "Next Match"
     (lambda () (interactive)
       (word-search-forward main-log-init-date)))
    ("<up>" "Prev Match"
     (lambda () (interactive)
       (word-search-backward main-log-init-date)))]]
  ["Ops"
   [("o" "cycle local" org-cycle)
    ("O" "cycle global" org-shifttab)]
   [;; ("C" "equalize" org-headings-equalize)
    ("-" "timelogrefile" org-timelogrefile-c)]])

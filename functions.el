;;; Functions
;;;; timestamp
(defun timestamp ()
  "Insert string for the current time."
  (interactive)
  (insert (format-time-string "[%02y-%02m-%02d %02H:%02M:%02S] ")))

(defun timestamp-no-time ()
  "timestamp without time"
  (interactive)
  (insert (format-time-string "[%02y-%02m-%02d]")))

;;;; convenience
(defun consult-ripgrep-local ()
  (interactive)
  (consult-ripgrep default-directory))

(defun consult-fd-local ()
  (interactive)
  (let ((consult-async-min-input 0))
    (consult-fd default-directory)))

(defun save-and-kill-buffer ()
  (interactive)
  (progn
    (save-buffer)
    (kill-current-buffer)))

(defun save-and-delete-window ()
  (interactive)
  (progn
    (save-buffer)
    (delete-window)))

(defun kill-current-buffer-and-window-c ()
  (interactive)
  (progn
    (kill-current-buffer)
    (delete-window)))

(defun revert-buffer-if-not-modified ()
  "Revert the buffer if it isn’t modified."
  (interactive)
  (let ((changed? (not (verify-visited-file-modtime))))
    (if (buffer-modified-p)
        (if changed?
            (message
             (concat
              (propertize "Buffer " 'face 'variable-pitch)
              (propertize (buffer-name) 'face '(variable-pitch bold font-lock-builtin-face))
              (propertize " changed on disk.  " 'face 'variable-pitch)
              (propertize "You have unsaved changes." 'face '(variable-pitch success)))))
      (if changed?
          (progn
            (message
             (concat
              (propertize "Buffer " 'face 'variable-pitch)
              (propertize (buffer-name) 'face '(variable-pitch bold font-lock-builtin-face))
              (propertize " changed on disk.  " 'face 'variable-pitch)
              (propertize "REVERTING." 'face '(variable-pitch success))))
            (revert-buffer t t))))))

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

(defun corfu-toggle-autocomplete ()
  (interactive)
  (setq corfu-auto (not corfu-auto))
  (if corfu-mode
      (progn
        (corfu-mode -1)
        (corfu-mode 1))))

(defun echo-current-buffer-path ()
  (interactive)
  (message (propertize (concat " " (if buffer-file-name buffer-file-name "nil") " ")
                       'face '( :inherit (font-lock-keyword-face bold)
                                :height 1.5
                                :box (list :line-width 1)))))

(defun copy-current-buffer-path-c ()
  (interactive)
  (let ((n (if buffer-file-name buffer-file-name "nil")))
    (kill-new n)
    (message (concat "Copied " (propertize n 'face 'success) " to clipboard."))))

(defun echo-current-time ()
  "Echo the current time"
  (interactive)
  (message (propertize (format-time-string "  %Y-%02m-%02d %02H:%02M:%02S %A Week[%U] Day[%j]  ")
                       'face '( :inherit (font-lock-keyword-face bold)
                                :height 1.95
                                :box (list :line-width 2)))))

(defun dired-jump-other-window ()
  "`dired-jump’ with OTHER-WINDOW as t."
  (interactive)
  (dired-jump t))

;; (defun clone-indirect-buffer-with-action (&optional action)
;;   (interactive)
;;   (let* ((newname  (buffer-name))
;;          ;; (newname (if (string-match "<[0-9]+>\\’" newname)
;;          ;;              (substring newname 0 (match-beginning 0))))
;;          (newname (generate-new-buffer-name newname))
;;          (newbuffer (make-indirect-buffer (current-buffer) newname)))
;;     (if action
;;         (pop-to-buffer newbuffer action t)
;;       (pop-to-buffer newbuffer nil t))
;;     (message (concat "New indirect buffer "
;;                      (propertize newname 'face 'success)
;;                      " created."))))

;; (defun clone-indirect-buffer-with-action-split ()
;;   (interactive)
;;   (clone-indirect-buffer-with-action 'display-buffer-below-selected))

(defun undo-with-prefix ()
  (interactive)
  (let ((current-prefix-arg '(4))
        (p (point))
        (m (mark)))
    (call-interactively #'undo)
    (set-mark m)
    (goto-char p)
    (call-interactively #'activate-mark)))

(defun undo-redo-with-prefix ()
  (interactive)
  (let ((current-prefix-arg '(4))
        (p (point))
        (m (mark)))
    (call-interactively #'undo-redo)
    (set-mark m)
    (goto-char p)
    (call-interactively #'activate-mark)))

(defun load-current-file ()
  (interactive)
  (load-file buffer-file-name))

(defun scroll-5l-down ()
  (interactive)
  (scroll-down 5))

(defun scroll-5l-up ()
  (interactive)
  (scroll-up 5))

(defun elisp-show-callable-definition-c ()
  (interactive)
  (helpful-callable (helpful--callable-at-point)))

(defun elisp-show-variable-definition-c ()
  (interactive)
  (helpful-variable (helpful--variable-at-point)))

(defun continue-structure-c ()
  "Continue a list / syntactic structure."
  (interactive)
  (let ((end (save-excursion (beginning-of-line-text) (point)))
        (beg (save-excursion (beginning-of-line) (point))))
    (when (> end beg)
      (end-of-line)
      (insert (concat "\n" (buffer-substring beg end))))))

(defun enable-hyper-key-c ()
  "Call XModMap to enable the HYPER Key."
  (interactive)
  (shell-command "setxkbmap ; xmodmap ~/.Xmodmap")
  (shell-command "xfconf-query -c keyboards -p /Default/KeyRepeat/Delay -s 90")
  (message (concat (propertize "HYPER" 'face 'font-lock-builtin-face)
                   (propertize " key enabled." 'face 'success))))

;;;; Exit Emacs
(defun clean-exit ()
  "Exit Emacs cleanly.
If there are unsaved buffer, pop up a list for them to be saved
before existing. Replaces ‘save-buffers-kill-terminal’.
From https://archive.casouri.cc/note/2021/clean-exit/index.html"
  (interactive)
  (if (frame-parameter nil 'client)
      (server-save-buffers-kill-terminal nil)
    (if-let* ((buf-list (seq-filter (lambda (buf)
                                      (and (buffer-modified-p buf)
                                           (buffer-file-name buf)))
                                    (buffer-list))))
        (progn
          (pop-to-buffer (list-buffers-noselect t buf-list))
          (message "s to save, C-k to kill, x to execute"))
      (save-buffers-kill-terminal))))

;;;; completion
;;;;; kill-new from global paste
(defun kill-new-from-global-paste-c ()
  (interactive)
  (require 'sqlite3)
  (let ((mylist '()))
    (unless (f-exists-p "/home/sys2/.local/share/zeitgeist/activity.sqlite")
      (user-error "Zeitgeist database does not exist."))
    (let* ((dbh (sqlite3-open "/home/sys2/.local/share/zeitgeist/activity.sqlite" sqlite-open-readonly))
           (stmt (sqlite3-prepare dbh "select * from text order by id desc limit 1000")))
      (while (= sqlite-row (sqlite3-step stmt))
        (cl-destructuring-bind (id text) (sqlite3-fetch stmt)
          (setq mylist (nconc mylist (list text)))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh))
    (kill-new
     (let* ((vertico-sort-function nil)
            (rlist (completing-read " " mylist))) ;; [23-12-15 00:48:07] Because I like my commas intact
       rlist))))

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

(defun char-to-unicode-insert ()
  (interactive)
  (insert (format "0x%x" (char-after (point)))))

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

;;;; transparency
(defvar transparency-value-c 0.7
  "Transparency value for `set-transparency-c’.")
(defun set-transparency-c (&optional value no-set-default only-active-frame)
  "Set the transparency of the selected window.
Needs frame-parameter alpha-background."
  (interactive)
  (let ((val (if value value
               (if transparency-value-c transparency-value-c
                 0.7))))
    (unless no-set-default
      (progn
        (let ((x (assoc 'alpha-background default-frame-alist)))
          (if x (delq x default-frame-alist)))
        (add-to-list 'default-frame-alist (cons 'alpha-background val))))
    (if only-active-frame
        (set-frame-parameter nil 'alpha-background val)
      (let ((f (frame-list)))
        (mapc #'(lambda (x) (set-frame-parameter x 'alpha-background val)) f)))))

(defun turn-off-transparency-c (&optional no-set-default only-active-frame)
  (interactive)
  (set-transparency-c 1.0 no-set-default only-active-frame))

;;;; Debug
(defun toggle-debug-mode ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message (concat
            "Debug mode set to "
            (propertize (format "%s" debug-on-error)
                        'face (if debug-on-error 'success 'error)))))

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

;;;; Profiler
(defun profiler-stop-and-report ()
  "Stop the profiler with `profiler-stop’ and initiate `profiler-report’."
  (interactive)
  (progn
    (funcall-interactively #'profiler-stop)
    (funcall-interactively #'profiler-report)))

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
  (vterm))

(defun open-shell-vsplit-c ()
  "Vsplit window and open shell"
  (interactive)
  (evil-window-vsplit)
  (vterm))

(defun open-external-term-here-c ()
  "Open `ter' in the current directory."
  (interactive)
  (shell-command (concat "xfce4-terminal --working-directory=\"" (file-truename default-directory) "\"")))

(defun vterm-new-instance ()
  "Open a new instance of vterm in the current window."
  (interactive)
  (vterm 't))

;;;; Frame Functions
(defun delete-frame-force-c ()
  "`delete-frame’ with FORCE set to t."
  (interactive)
  (delete-frame nil t))

;;;; Window Functions
;; It's like golden-ratio but just one function.
;; width, then height.
(defun make-window-larger-c (&optional ratio)
  "Makes the current window larger or smaller.
RATIO is the ratio used, default is 0.618.
It switches the width before the height."
  (interactive)
  (save-excursion
    (let* (;; (postv      (if olivetti-mode
           ;;                 (progn (olivetti-mode -1) t)
           ;;               nil))
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
      ;; (if postv (olivetti-mode 1))
      )))

(defun split-enlarge-follow-mode-toggle nil
  "Split window, enlarge left and then enable follow-mode."
  (interactive)
  (progn
    (call-interactively #'evil-window-vsplit)
    (call-interactively #'evil-window-left)
    (call-interactively #'follow-mode 1)))

;;;; org-*
;;;;; org-id-*
(defun org-id-create-insert (&optional kill?)
  "Create an Org ID and insert it.  KILL? to `kill-new’ instead."
  (interactive)
  (let* ((heading (completing-read
                   "Heading: "
                   (org-map-entries ;; Also tried org-element-map and others, didn’t work.
                    (lambda ()
                      (format "%s %s" (point) (org-get-heading t t t t))))))
         (sep (string-match " " heading))
         (p (string-to-number (substring heading 0 sep)))
         ;; (heading (substring heading (+ 1 sep)))
         (orgid nil))
    (save-excursion
      (save-restriction
        (goto-char p)
        (setq orgid (org-id-get (point) t))))
    (funcall (if kill? #'kill-new #'insert) (format " [[%s]]" orgid))))

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

;;;;; org-insert-block-*
(defun org-insert-block-c (type &optional metadata?)
  "Insert block TYPE at point, or enclosing the current region.
Set METADATA? to t if you need to enter text after #+begin_TYPE."
  (let ((bounds (if (region-active-p) (region-bounds))))
    (if bounds
        (progn
          (save-excursion
            (goto-char (cdar bounds))
            ;; (end-of-line) ;; Region reports one int above end, maybe evil-specific?
            (insert (format "#+end_%s\n" type))
            (goto-char (caar bounds))
            (beginning-of-line)
            (insert (format "#+begin_%s\n" type)))
          (if metadata? (goto-char (caar bounds))))
      (save-excursion
        (end-of-line)
        (insert (format "\n#+begin_%s\n#+end_%s" type type))))
    (if metadata?
        (progn
          (unless bounds (next-line))
          (save-excursion
            (end-of-line)
            (insert " "))
          (if (and (boundp 'evil-mode) evil-mode)
              (evil-append-line nil)
            (end-of-line))))))

(defun org-insert-block-quote-c ()
  "Insert a Quote block at point, or enclosing the current region."
  (interactive)
  (org-insert-block-c "quote"))

(defun org-insert-block-src-c ()
  "Insert a Source block at point, or enclosing the current region."
  (interactive)
  (org-insert-block-c "src"))

(defun org-insert-block-quote-meta-c ()
  "Insert a Quote block at point, or enclosing the current region.
Then place point at end of #+begin statement for metadata insertion."
  (interactive)
  (org-insert-block-c "quote" t))

(defun org-insert-block-src-meta-c ()
  "Insert a Source block at point, or enclosing the current region.
Then place point at end of #+begin statement for metadata insertion."
  (interactive)
  (org-insert-block-c "src" t))

(defun org-insert-block-custom-c (block)
  "Insert a custom block at point, or enclosing the current region."
  (interactive "sBlock: ")
  (org-insert-block-c block t))

(defun org-insert-block-custom-meta-c (block)
  "Insert a custom block at point, or enclosing the current region.
Then place point at end of #+begin statement for metadata insertion."
  (interactive "sBlock: ")
  (org-insert-block-c block t))

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
   ;; [("oo" (lambda () (toggle-modes-transient--description 'perfect-margin-mode "Perfect Margin "))
   ;;   perfect-margin-mode)]
   [("oo" (lambda () (toggle-modes-transient--description 'olivetti-mode "Olivetti "))
     olivetti-mode :if (lambda () (fboundp #'olivetti-mode)))]
   [("tl" (lambda () (toggle-modes-transient--description 'truncate-lines "Truncate Lines"))
     toggle-truncate-lines)
    ("v" (lambda () (toggle-modes-transient--description 'visual-line-mode "Visual Lines"))
     visual-line-mode)]
   [("ll" (lambda () (toggle-modes-transient--description 'display-line-numbers "Line Numbers")) display-line-numbers-mode)
    ("lL" (lambda () (toggle-modes-transient--description 'global-display-line-numbers-mode "Global Line Numbers")) global-display-line-numbers-mode)
    ("lg" (lambda () (toggle-modes-transient--description 'diff-hl-mode "Diff Highlight")) diff-hl-mode)]
   [;; ("i" (lambda () (toggle-modes-transient--description 'indent-bars-mode "Indent Guides")) indent-bars-mode)
    ;; [("i" (lambda () (toggle-modes-transient--description 'highlight-indent-guides-mode "Indent Guides")) highlight-indent-guides-mode)
    ("m" (lambda () (toggle-modes-transient--description 'mixed-pitch--applied-p "Mixed Pitch"))
     mixed-pitch-mode)
    ("L" (lambda () (toggle-modes-transient--description 'hl-line-mode "Highlight Lines"))
     hl-line-mode)]]
  ["Modes and Utility"
   ["Major Modes"
    ("0" "Clean Mode" clean-mode)
    ("1" "Fundamental Mode" fundamental-mode)]
   ["Alarm"
    ("-aa" "Set Alarm" alarm-clock-set)
    ("-al" "List Alarms" alarm-clock-list-view)
    ("-ak" "Kill Alarms" alarm-clock-kill)]
   ["Desktop"
    ("-ds" "Save Desktop" desktop-save-in-desktop-dir)
    ("-dr" "Read Desktop" desktop-read)]]
  ["Other"
   [("B" "Revert Buffer" revert-buffer)
    ("z" "*scratch*" scratch-buffer)
    ("F" "Find File" find-file)]
   [("#" "Open Terminal here" open-external-term-here-c :transient nil)
    ("sv" "Open VTerm here" project-vterm :transient nil)
    ("se" "Open EShell here" project-eshell :transient nil)]
   [("$$" "Highlight regexp" highlight-regexp :transient nil) ;;buffer operations DO NOT WORK
    ("%%" "Highlight phrase" highlight-phrase :transient nil)
    ("$%" "Unhighlight regexp/phrase" unhighlight-regexp :transient nil)]
   [("tt" "Turn on Transparency" (lambda () (interactive) (set-transparency-c nil t t)))
    ("tT" "Turn off Transparency" (lambda () (interactive) (turn-off-transparency-c nil t)))
    ("t!" (lambda (x) (interactive "sValue? ") (setq transparency-value-c (string-to-number x)))
     :description (lambda () (interactive)
                    (concat "Transparency var: "
                            (propertize (format "%s" transparency-value-c)
                                        'face 'diary))))]
   [("tg" "Turn on Transparency (global)" (lambda () (interactive) (set-transparency-c nil nil nil)))
    ("tG" "Turn off Transparency (global)" (lambda () (interactive) (turn-off-transparency-c nil nil)))]]
  ["Misc"
   ["Corfu"
    ("cc" (lambda () (toggle-modes-transient--description 'corfu-mode "Corfu")) corfu-mode)
    ("cC" (lambda () (toggle-modes-transient--description 'corfu-auto "Autocomplete")) corfu-toggle-autocomplete)
    ("co" (lambda () (toggle-modes-transient--description 'corfu-candidate-overlay-mode "Candidate Overlay")) corfu-candidate-overlay-mode)]
   [("<f9>" "Save Buffer" save-buffer)
    ("T" "Switch theme" load-theme)]]
  ["+"
   [("+" "New Id" (lambda () (interactive) (find-file "~/46/da/id")) :transient nil)
    ("<return>" "New TimeLog" new-timelog-c :transient nil)]
   [("--" "Open da.org" (lambda () (interactive) (find-file "~/46/da/da.org")) :transient nil)]
   [("r" "Refresh Agenda" (lambda () (interactive)
                            (progn
                              (let ((filelist (directory-files-recursively
                                               "~/46/da/timelog" (rx ".org" eos))))
                                (setq org-agenda-files nil)
                                (setq org-agenda-files filelist)
                                (message "Agenda refreshed")))))
    ("." "New TimeLog Log" org-todo-today-timestamp-c :transient nil)]
   [("/" "New TimeLog TODO" org-todo-today-timestamp-todo-c :transient nil)
    ("a" "Agenda+Todo" (lambda () (interactive) (org-agenda nil "o")) :transient nil)]
   ]
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
           (let ((datevar (read-string "Init Date: "
                                       (when (boundp 'main-log-init-date)
                                         main-log-init-date))))
             (setq main-log-init-date datevar)))
     :description (lambda () (interactive)
                    (unless (boundp 'main-log-init-date)
                      (setq main-log-init-date (format-time-string
                                                "%y-%m-%d")))
                    (concat "Current Date: "
                            (propertize main-log-init-date
                                        'face 'font-lock-keyword-face))))
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
       (let* ((daycount '((01 . 31) (02 . 29) (03 . 31) (04 . 30)
                          (05 . 31) (06 . 30) (07 . 31) (08 . 31)
                          (09 . 30) (10 . 31) (11 . 30) (12 . 31)))
              (year (string-to-number (substring main-log-init-date 0 2)))
              (month (string-to-number (substring main-log-init-date 3 5)))
              (day (string-to-number (substring main-log-init-date 6 8)))
              (incmonth (if (>= day (cdr (assoc month daycount))) t))
              (incyear (if (and incmonth (>= month 12)) t))
              (day (if incmonth 1 (1+ day)))
              (month (if incmonth (1+ month) month))
              (year (if incyear (1+ year) year)))
         (setq main-log-init-date
               (format "%02d-%02d-%02d" year month day)))))
       ;; (let ((newlast (+ 1 (string-to-number
       ;;                      (substring main-log-init-date 6 8)))))
       ;;   (setq main-log-init-date
       ;;         (concat (substring main-log-init-date 0 6)
       ;;                 (if (= (length (number-to-string newlast)) 1)
       ;;                     "0")
       ;;                 (number-to-string newlast))))
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
       (search-forward main-log-init-date)))
    ("<up>" "Prev Match"
     (lambda () (interactive)
       (search-backward main-log-init-date)))]]
  ["Ops"
   [("o" "cycle local" org-cycle)
    ("O" "cycle global" org-shifttab)]
   [;; ("C" "equalize" org-headings-equalize)
    ("-" "timelogrefile" org-timelogrefile-c)
    ("J05" "timelogrefile next 5" (lambda () (interactive)
                                   (dotimes (x 5)
                                     (org-timelogrefile-c)
                                     (next-line))))
    ("J15" "timelogrefile next 15" (lambda () (interactive)
                                     (dotimes (x 15)
                                       (org-timelogrefile-c)
                                       (next-line))))
    ("J25" "timelogrefile next 25" (lambda () (interactive)
                                     (dotimes (x 25)
                                       (org-timelogrefile-c)
                                       (next-line))))
    ("J50" "timelogrefile next 50" (lambda () (interactive)
                                     (dotimes (x 50)
                                       (org-timelogrefile-c)
                                       (next-line))))
    ]]
  )

;;;; org-agenda
;;;;; New timelog file
(defun new-timelog-c ()
  "Make/Open today's timelog file"
  (interactive)
  (let* ((givendate (format-time-string "%y-%m-%d"))
         (filedate (format-time-string "%y%m%d"))
         (basefile (concat "~/46/da/timelog/" filedate ".log.org")))
    (if (file-directory-p basefile)
        (progn
          (find-file basefile)
          (write-region (concat "#+TITLE: [" givendate "]\n\n")
                        nil basefile t)
          (end-of-buffer))
      (progn
        (find-file basefile)
        (end-of-buffer)))))

;;;;; todo timestamp
(defun org-todo-timestamp-c (&optional todo)
  "Insert new heading with timestamp and open line below."
  (interactive)
  (progn
    (goto-char (point-max))
    (if todo (insert "\n* TODO [#B]\nOPENED: ") (insert "\n* "))
    (timestamp)
    (newline)))

(defun org-todo-timestamp-todo-c ()
  "Insert new TODO heading with timestamp and open line below."
  (interactive)
  (org-todo-timestamp-c t))

(defun org-todo-today-timestamp-c ()
  "Open today's file and execute `org-todo-timestamp-c'"
  (interactive)
  (find-file (concat "~/46/da/timelog/"
                     (format-time-string "%y%m%d") ".log.org"))
  (org-todo-timestamp-c))

(defun org-todo-today-timestamp-todo-c ()
  "Open today's file and execute `org-todo-timestamp-todo-c'"
  (interactive)
  (find-file (concat "~/46/da/timelog/"
                     (format-time-string "%y%m%d") ".log.org"))
  (org-todo-timestamp-todo-c))

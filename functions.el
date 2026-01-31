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
(defun consult-ripgrep-local (&optional dir initial)
  "Run ripgrep on the current directory, nothing ignored."
  (interactive)
  (let ((consult-async-min-input 1)
        (consult-ripgrep-args (concat consult-ripgrep-args " -uu")))
    (consult-ripgrep (or dir default-directory) initial)))

(defun consult-fd-local (&optional dir initial)
  "Run fdfind in the current directory, nothing ignored.
DIR is the directory, INITIAL is the string."
  (interactive)
  (let ((consult-async-min-input 1)
        (consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd") "--full-path --color=never -u")))
    (consult-fd (or dir default-directory) initial)))

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
  (let* ((b (window-buffer (selected-window)))
         (changed? (not (verify-visited-file-modtime b))))
    (with-current-buffer b
      (if (buffer-modified-p)
          (when changed?
            (message
             (concat
              (propertize "Buffer " 'face 'variable-pitch)
              (propertize (buffer-name) 'face '(variable-pitch bold font-lock-builtin-face))
              (propertize " changed on disk.  " 'face 'variable-pitch)
              (propertize "You have unsaved changes." 'face '(variable-pitch success)))))
        (when changed?
          (message
           (concat
            (propertize "Buffer " 'face 'variable-pitch)
            (propertize (buffer-name) 'face '(variable-pitch bold font-lock-builtin-face))
            (propertize " changed on disk.  " 'face 'variable-pitch)
            (propertize "REVERTING." 'face '(variable-pitch success))))
          (revert-buffer t t))))))

(defun toggle-eldoc-box ()
  (interactive)
  (if eldoc-box-hover-mode
      (eldoc-box-hover-mode -1)
    (eldoc-box-hover-mode t)))

(defun corfu-toggle-autocomplete ()
  (interactive)
  (setq corfu-auto (not corfu-auto))
  (if corfu-mode
      (progn
        (corfu-mode -1)
        (corfu-mode 1))))

(defun echo-current-buffer-path (arg)
  "Echo current buffer path.
If ARG is non-nil, kill path."
  (interactive "P")
  (let ((p (or buffer-file-name default-directory)))
    (if p (progn (when arg (kill-new p)) (message p))
      (message "`buffer-file-name’ and `default-directory’ is nil."))))

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

(defun clipboard-kill-ring-save-c ()
  "Run `clipboard-kill-ring-save’ if a region is active.
If region is inactive, then copy from kill-ring to clipboard."
  (interactive)
  (if (use-region-p) ;; not `region-active-p’
      (clipboard-kill-ring-save (region-beginning) (region-end))
    (when kill-ring (gui-set-selection 'CLIPBOARD (current-kill 0 t)))))

(defun fill-region-custom-width-c (arg)
  "Run `fill-region’ with a custom `fill-column’ value (from ARG or otherwise)."
  (interactive "P")
  (if (use-region-p)
      (let ((fill-column (if arg (prefix-numeric-value arg)
                           (read-number "Fill Column Width:" 75)))
            (justify (read-number "Justify [0]none [1]Full [2]Left [3]Right [4]Center: " 0)))
        (fill-region (region-beginning) (region-end)
                     (pcase justify (0 nil) (1 'full) (2 'left) (3 'right) (4 'center))))
    (if (derived-mode-p 'prog-mode)
        (funcall-interactively #'prog-fill-reindent-defun)
      (fill-paragraph))))

(defun export-current-buffer-as-text (&optional buf)
  "Export current buffer as text for ingestion in other programs.
Copies to system clipboard.
When BUF is a buffer, return contents of buffer."
  (interactive)
  (let* ((b? (when buf (bufferp buf)))
         (s-buf (if b? buf (current-buffer)))
         (text (with-current-buffer s-buf
                 (buffer-substring-no-properties (point-min) (point-max))))
         (name (or (buffer-file-name s-buf) (buffer-name s-buf) "Unknown"))
         (bufsizekb (/ (buffer-size s-buf) 125.0))
         (proglang (substring (format "%s" major-mode) 0 -5))
         (outstring (format "--- SIZE: %.1fkb FILE: %s ---\n```%s\n%s\n```" bufsizekb name proglang text)))
    (if text (if b? (format "%s\n" outstring)
               (with-temp-buffer
                 (insert outstring)
                 (clipboard-kill-ring-save (point-min) (point-max)))
               (message (format "Saved file %s to Clipboard." name)))
      (unless b? (message (format "Could not extract text from Buffer %s." s-buf))))))

(defun export-selected-buffers-as-text ()
  "Export all buffers selected by user as text for ingestion in other programs.
Copies to system clipboard."
  (interactive)
  (let ((buflist (completing-read-multiple
                  "Buffers:" (mapcar #'buffer-name (buffer-list)) nil t)))
    (cl-loop for b in buflist
             concat (export-current-buffer-as-text (get-buffer b)) into text
             finally (with-temp-buffer
                       (insert text)
                       (clipboard-kill-ring-save (point-min) (point-max))))))

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
(defun kill-new-from-global-paste-c (arg)
  "Select and kill a new entry from the clipboard, which is managed by zeitgeist.
Set limit ARG."
  (interactive "P")
  (require 'sqlite3)
  (let ((mylist '())
        (db (expand-file-name "~/.local/share/zeitgeist/activity.sqlite"))
        (limit (if arg (prefix-numeric-value arg) 1000)))
    (unless (f-exists-p db)
      (user-error "Zeitgeist database does not exist."))
    (let* ((dbh (sqlite3-open db sqlite-open-readonly))
           (stmt (sqlite3-prepare dbh (format "select value from text order by id desc limit %s" limit))))
      (while (= sqlite-row (sqlite3-step stmt))
        (setq mylist (nconc mylist (list (sqlite3-fetch stmt)))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh))
    (kill-new
     (let* ((vertico-sort-function nil)
            (rlist (completing-read " " mylist))) ;; [23-12-15 00:48:07] Because I like my commas intact
       rlist))))

;;;; unicode
(defun insert-base16-char-c ()
  "Insert a base16 char.  SPC terminates input."
  (interactive)
  (cl-loop for k = (read-key) until (eq k ? )
           concat (char-to-string k) into s
           finally (unless (string-empty-p s) (insert-char (string-to-number s 16)))))

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
(defvar transparency-value-c 97
  "Transparency value for `set-transparency-c’.")

(defun set-transparency-c (arg &optional all-frames?)
  "Set the transparency of the current, or all, frames.
If ARG is provided, set (min 100 ARG) as the transparency value.
When ALL-FRAMES? is non-nil, change for all frames."
  (interactive "P")
  (let ((tval (if arg (min 100 (abs (prefix-numeric-value arg))) transparency-value-c)))
    (if all-frames?
        (modify-all-frames-parameters `((alpha-background . ,tval)))
      (modify-frame-parameters nil `((alpha-background . ,tval))))))

(defun set-transparency-watch-function-c (_ newval operation _)
  (when (eq operation 'set) (set-transparency-c newval t)))

(add-variable-watcher 'transparency-value-c #'set-transparency-watch-function-c)

;;;; Debug
(defun toggle-debug-mode ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message (concat
            "Debug mode set to "
            (propertize (format "%s" debug-on-error)
                        'face (if debug-on-error 'success 'error)))))

;; [25-06-26 23:26:50]  Moved to temper-width (previously squish-content)
;; ;;;; Center window content
;; (defvar center-window-content-width 125)
;; (defvar-local center-window-content-enabled nil)
;; (defvar center-window-content-allowed-modes
;;   '(text-mode dired-mode magit-mode Info-mode helpful-mode shortdoc-mode org-mode))
;; (defvar center-window-content-allow-prog-mode t)
;; (defun center-window-content (&optional w)
;;   "Center/Uncenter the contents of the window W or the current window.
;; Like Olivetti but without the invasive hooks."
;;   (interactive)
;;   (when (or (memq major-mode center-window-content-allowed-modes)
;;             (when center-window-content-allow-prog-mode (derived-mode-p 'prog-mode)))
;;     (let* ((win (or w (selected-window)))
;;            (tw (with-selected-window win (window-total-width)))
;;            ;; (bw (with-selected-window win (window-body-width)))
;;            (uw center-window-content-width)
;;            (err 2)
;;            (margin (/ (abs (- tw uw)) 2)))
;;       ;; (message "win:%s tw:%s uw:%s margin:%s" win tw uw margin)
;;       (when (> (abs (- tw uw)) err)
;;         (if (> tw uw)
;;             (progn (setq center-window-content-enabled t)
;;                    (set-window-margins win margin margin))
;;           (setq center-window-content-enabled nil)
;;           (set-window-margins win 0 0))))
;;     ;; (when center-window-content-enabled
;;     ;;   (setq center-window-content-enabled nil)
;;     ;;   (set-window-margins win 0 0))
;;     ))

;; ;; (when (> (abs (- tw uw)) err)
;; ;;       (if (> tw uw)
;; ;;           (if center-window-content-enabled
;; ;;               (when (> (abs (- bw uw)) err)
;; ;;                 (if (> bw uw)
;; ;;                     (set-window-margins win margin margin)
;; ;;                   (set-window-margins win 0 0)))
;; ;;             (setq center-window-content-enabled t)
;; ;;             (set-window-margins win margin margin))
;; ;;         (setq center-window-content-enabled nil)
;; ;;         (set-window-margins win 0 0)))

;; (defun center-window-content-frame (&optional frame)
;;   "Invoke `center-window-content’ on all windows in the current frame or FRAME."
;;   (interactive)
;;   ;; (message "------")
;;   (with-selected-frame (or frame (selected-frame))
;;     (walk-windows #'center-window-content)))

;; (remove-hook 'window-size-change-functions #'center-window-content-frame)
;; (remove-hook 'change-major-mode-hook #'center-window-content-frame)

;;;; with-at-point
(defun get-thing-at-point-or-region-c (thing)
  "Get THING with `thing-at-point’ or get the active region, and return it."
  (interactive)
  (let ((s (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (thing-at-point thing t))))
    (if (and s (not (string-empty-p s))) s nil)))

(defun consult-ripgrep-at-point-c ()
  "Run `consult-ripgrep’ with the symbol at point, or the active region."
  (interactive)
  (when-let* ((z (get-thing-at-point-or-region-c 'symbol)))
    (consult-ripgrep nil z)))

(defun consult-ripgrep-local-at-point-c ()
  "Run `consult-ripgrep-local’ with the symbol at point, or the active region."
  (interactive)
  (when-let* ((z (get-thing-at-point-or-region-c 'symbol)))
    (consult-ripgrep-local default-directory z)))

(defun consult-line-at-point-c ()
  "Run `consult-line’ with the symbol at point, or the active region."
  (interactive)
  (when-let* ((z (get-thing-at-point-or-region-c 'symbol)))
    (consult-line z)))

;; (defun project-find-file-at-point-c ()
;;   "Run `project-find-file’ with the symbol at point, or the active region."
;;   (interactive)
;;   (when-let* ((z (get-thing-at-point-or-region-c 'symbol))
;;               (pr (project-current t))
;;               (root (project-root pr)))
;;     (project-find-file-in z (list root) pr t)))

(defun consult-fd-local-global-at-point-c ()
  "Run `consult-fd-local’ with the symbol at point, or the active region, in the current project."
  (interactive)
  (when-let* ((z (get-thing-at-point-or-region-c 'symbol)))
    (consult-fd-local (project-root (project-current t)) z)))

(defun consult-fd-local-at-point-c ()
  "Run `consult-fd-local’ with the symbol at point, or the active region, in `default-directory’."
  (interactive)
  (when-let* ((z (get-thing-at-point-or-region-c 'symbol)))
    (consult-fd-local default-directory z)))

;;;;; highlight
(defvar highlight-at-point-faces-c '( hi-yellow hi-blue hi-pink hi-green hi-salmon hi-aquamarine
                                      highlight lazy-highlight))
(defvar-local highlight-at-point-used-faces-c 0)
(defun highlight-at-point-c (arg)
  "Run `highlight-phrase’ with the symbol at point, or the active region, in the current buffer.
\\[universal-argument]: unhighlight at point
\\[universal-argument] \\[universal-argument]: unhighlight all in buffer."
  (interactive "P")
  (cond
   ((null arg)
    (when-let* ((z (get-thing-at-point-or-region-c 'symbol))
                (f (nth highlight-at-point-used-faces-c highlight-at-point-faces-c)))
      (when f (cl-incf highlight-at-point-used-faces-c))
      (highlight-phrase (if (use-region-p) z (concat "\\<" (regexp-quote z) "\\>")) f)
      (message (format "Highlighting %s" (if (use-region-p) "region" (propertize z 'face f))))))
   ((equal arg '(4))
    (when-let* ((z (get-thing-at-point-or-region-c 'symbol))
                (z (concat "\\<" (regexp-quote z) "\\>")))
      (when (> highlight-at-point-used-faces-c 0) (decf highlight-at-point-used-faces-c))
      (hi-lock-unface-buffer z)))
   (t (progn
        (let ((z highlight-at-point-used-faces-c))
          (unhighlight-all-in-buffer-c)
          (message (format "%s highlights removed." z)))))))

(defun unhighlight-all-in-buffer-c ()
  "Run `hi-lock-unface-buffer’ and zero `highlight-at-point-used-faces-c’."
  (setq highlight-at-point-used-faces-c 0)
  (hi-lock-unface-buffer t))

;;;;; Color
(defun convert-color-string-at-point-to-rgb (arg)
  "Get the color string (symbol) at point and convert it to RGB (0-255).
When ARG is non-nil, replace the symbol at point with the new string.
Otherwise, kill it." ;; Can use `color-values’ instead.
  (interactive "P")
  (when-let* ((sym (thing-at-point 'symbol t))
              (s (if (s-suffix? "\"" sym) (substring s 0 -1) sym)) ;; not strictly necessary
              (s (if (s-prefix? "\"" s) (substring s 1 0) s))
              (s (if (s-prefix? "#" s) (substring s 1 0) s))
              (r (substring s 0 2))
              (g (substring s 2 4))
              (b (substring s 4 6)))
    ((lambda (z) (if arg (let ((b (bounds-of-thing-at-point 'symbol)))
                           (save-excursion (goto-char (car b))
                                           (delete-region (car b) (cdr b))
                                           (insert z)))
                   (message z)))
     (thread-last (list r g b)
                  (mapcar (lambda (y) (string-to-number y 16)))
                  (format "%s")))))

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
   [("tt" "Turn on Transparency" (lambda () (interactive) (set-transparency-c nil)))
    ("tT" "Turn off Transparency" (lambda () (interactive) (set-transparency-c 100)))
    ("t!" (lambda (x) (interactive "sValue? ") (setq transparency-value-c (string-to-number x)))
     :description (lambda () (interactive)
                    (concat "Transparency var: "
                            (propertize (format "%s" transparency-value-c)
                                        'face 'diary))))]
   [("tg" "Turn on Transparency (global)" (lambda () (interactive) (set-transparency-c nil t)))
    ("tG" "Turn off Transparency (global)" (lambda () (interactive) (set-transparency-c 100 t)))]]
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

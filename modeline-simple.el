;;;; Header Line
;;;;; functions
(defun get-color-fg (face &optional fallback)
  "Get the :foreground property of a FACE.
If FACE is nil, set to FALLBACK."
  (let ((face? (face-attribute face :foreground)))
    (if (or (eq nil face?) (string-equal "unspecified" face?))
        (if fallback
            (face-attribute fallback :foreground)
          face?)
      face?)))

(defun get-color-bg (face &optional fallback)
  "Get the :background property of a FACE.
If FACE is nil, set to FALLBACK."
  (let ((face? (face-attribute face :background)))
    (if (or (eq nil face?) (string-equal "unspecified" face?))
        (if fallback
            (face-attribute fallback :background)
          face?)
      face?)))

(defun mix-colors (x y &optional ratio)
  "Mix two RGB Colors `X’ and `Y’ (represented as #RRGGBB) together by `RATIO'.
Very naive mixer.  Moves towards white for ratio>=0.5 ."
  (let* ((x (substring x 1))
         (y (substring y 1))
         (d (if ratio ratio 0.5))
         (r (+ (* d (string-to-number (substring x 0 2) 16))
               (* d (string-to-number (substring y 0 2) 16))))
         (g (+ (* d (string-to-number (substring x 2 4) 16))
               (* d (string-to-number (substring y 2 4) 16))))
         (b (+ (* d (string-to-number (substring x 4) 16))
               (* d (string-to-number (substring y 4) 16))))
         (r (if (< 255 r) 255 (truncate r)))
         (g (if (< 255 g) 255 (truncate g)))
         (b (if (< 255 b) 255 (truncate b))))
    (format "#%02x%02x%02x" r g b)))

(defvar headerline-active-window (selected-window))

(defun headerline-set-active-window ()
  "Set hte active window in var `headerline-active-window’."
  (setq headerline-active-window (selected-window)))

(defun headerline-active ()
  "Return non-nil if the selected window has an active headerline."
  (eq (selected-window) headerline-active-window))

(defvar-local headerline-buffer-id-cache-c nil)

(defun headerline-generate-buffer-id-cache-c ()
  (while-no-input
    (when after-init-time
      (setq headerline-buffer-id-cache-c
            (if buffer-file-truename
                (let ((g-root (locate-dominating-file buffer-file-truename ".git")))
                  (if (not g-root)
                      nil
                    (let* ((buf (file-truename buffer-file-truename))
                           (gitroot (file-truename g-root))
                           (gitroot-nodash (substring gitroot 0 -1))
                           (gitroot-a (replace-regexp-in-string
                                       "^[z-a]*/" "" gitroot-nodash))
                           (dirbefore (substring
                                       gitroot-nodash
                                       0
                                       (* -1
                                          (+ 1
                                             (length gitroot-a))))))
                      (cons gitroot-a
                            (substring buf
                                       (length gitroot)
                                       nil)))))
              (if (eq major-mode 'dired-mode)
                  (cons nil dired-directory)
                nil))))))
(dolist (hook '(change-major-mode-after-body-hook
                after-save-hook ;; In case the user saves the file to a new location
                focus-in-hook ;; ...or makes external changes then returns to Emacs
                after-set-visited-file-name-hook ;; ...when the visited file changes (e.g. it's renamed)
                after-revert-hook)) ;; ...when the underlying file changes
  (add-hook hook 'headerline-generate-buffer-id-cache-c))

(defun headerline-flymake-count-c (type)
  "Get Error/Warning/Note counts from Flycheck.
Modified from `flymake--mode-line-counter'.
`type' can be `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (number-to-string count)))

;;;;; Faces
(defface headerline-base-face
  '((t :foreground "#000"
       :background "#000"))
  "Base Face for Headerline")
(defface headerline-active-indicator-face
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for active window.")
(defface headerline-inactive-indicator-face
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for inactive window.")
(defface headerline-file-modified-face
  '((t :foreground "#000"))
  "File Modified Face for Headerline")
(defface headerline-file-unmodified-face
  '((t :foreground "#000"))
  "File Unmodified Face for Headerline")
(defface headerline-narrow-indicator-face
  '((t :foreground "#000"))
  "Face for narrowing.")
(defface headerline-buffer-parent-name-face
  '((t :foreground "#000"))
  "Face for parent in buffer name.")
(defface headerline-buffer-file-name-face
  '((t :foreground "#000"))
  "Face for file in buffer name.")
(defface headerline-major-mode-face
  '((t :foreground "#000"))
  "Face for major mode.")
(defface headerline-buffer-status-face
  '((t :foreground "#000"))
  "Face for buffer status.")
(defface headerline-match-face
  '((t :foreground "#000"))
  "Face for match numbers.")
(defface headerline-macro-face
  '((t :foreground "#000"))
  "Face for indicating macros.")
(defface headerline-dark-face
  '((t :foreground "#000"))
  "Dark face.")

(defun set-headerline-faces (&rest rest)
  (interactive)
  (let* ((fl-keyword (get-color-fg 'font-lock-keyword-face)) ;; green
         (fl-builtin (get-color-fg 'font-lock-builtin-face)) ;; blue
         (fl-constant (get-color-fg 'font-lock-constant-face)) ;; yellow
         (fl-type (get-color-fg 'font-lock-type-face)) ;; brown
         (fl-variable (get-color-fg 'font-lock-punctuation-face)) ;; off-white
         (fl-doc (get-color-fg 'font-lock-doc-face)) ;; red
         (region (get-color-bg 'region)) ;; brown
         (fl-regexp (get-color-fg 'font-lock-regexp-face 'font-lock-string-face)) ;; dark red
         (errorface (get-color-fg 'error)) ;; red
         (matchface (get-color-fg 'match)) ;; green
         ;; (warnface (get-color-fg 'font-lock-warning-face)) ;; red-ish
         (warnface (get-color-fg 'font-lock-type-face)) ;; red-ish
         (noteface (get-color-bg 'hi-yellow)) ;; yellow
         (defaultfg (get-color-fg 'default)) ;; white
         (defaultbg (get-color-bg 'default)) ;; black
         (height 1.2)
         (mix1 (mix-colors (if (not (eq 'unspecified fl-variable)) fl-variable fl-type) region 0.6)))
    (set-face-attribute 'headerline-base-face nil
                        :box (list :line-width '(1 . 3) :color region)
                        :height height
                        :foreground fl-variable
                        :background region)
    (set-face-attribute 'headerline-active-indicator-face nil
                        :box (list :line-width '(5 . 3) :color region)
                        :height height
                        :foreground mix1
                        :background mix1)
    (set-face-attribute 'headerline-inactive-indicator-face nil
                        :box (list :line-width '(5 . 3) :color region)
                        :height height
                        :foreground region
                        :background region)
    (set-face-attribute 'headerline-file-modified-face nil
                        :foreground fl-builtin)
    (set-face-attribute 'headerline-file-unmodified-face nil
                        :foreground fl-keyword)
    (set-face-attribute 'headerline-narrow-indicator-face nil
                        :foreground fl-regexp
                        :weight 'bold
                        :inherit 'variable-pitch)
    (set-face-attribute 'headerline-buffer-parent-name-face nil
                        :foreground mix1
                        :inherit 'variable-pitch
                        :weight 'bold
                        :height 1.15)
    (set-face-attribute 'headerline-buffer-file-name-face nil
                        :foreground mix1
                        :inherit 'variable-pitch
                        :height 1.15)
    (set-face-attribute 'headerline-major-mode-face nil
                        :foreground mix1
                        :inherit 'variable-pitch
                        :height 1.2
                        :weight 'bold)
    (set-face-attribute 'headerline-buffer-status-face nil
                        :foreground (mix-colors region fl-keyword)
                        :height 1.2)
    (set-face-attribute 'headerline-match-face nil
                        :foreground fl-keyword
                        :weight 'bold
                        :height 1.15)
    (set-face-attribute 'headerline-macro-face nil
                        :foreground fl-constant
                        :inherit 'variable-pitch
                        :height 1.2)
    (set-face-attribute 'headerline-dark-face nil
                        :foreground (mix-colors region fl-constant 0.35)
                        :weight 'bold)
    (defvar headerline--err-face (if errorface (mix-colors region errorface 0.45) "#000000"))
    (defvar headerline--warn-face (if warnface (mix-colors region warnface 0.45) "#000000"))
    (defvar headerline--note-face (if noteface (mix-colors fl-variable noteface 0.3) "#000000"))
    (defvar headerline--default-face (if defaultfg (mix-colors region defaultfg) "#000000"))
    (if (and (mlscroll-mode) (boundp 'mlscroll-in-color) (boundp 'mlscroll-out-color))
        (progn
          (setq-default mlscroll-in-color (cl-reduce #'mix-colors (list region (if (not (eq 'unspecified fl-doc)) fl-doc errorface) fl-keyword)))
          (setq-default mlscroll-out-color defaultbg)
          (mlscroll-mode -1) (mlscroll-mode 1)))
    (update-face-remapping-alist 'headerline-base-face 'header-line)))

(defun update-face-remapping-alist (face target)
  (unless (member face face-remapping-alist)
    (push (list target face) face-remapping-alist)))

(set-headerline-faces) ;; init during load
(add-to-list 'enable-theme-functions 'set-headerline-faces)
(if (fboundp 'set-fonts-c) (add-to-list 'enable-theme-functions 'set-fonts-c))

;;;;; Headerline Constructs
(defun headerline-buffer-status-c ()
  "Buffer RO/modified/none"
  `(:eval
    (cond (buffer-read-only (propertize " ✖ " 'face 'headerline-buffer-status-face))
          ((buffer-modified-p) (propertize " ⬤ " 'face 'headerline-buffer-status-face))
          (t (propertize "   " 'face 'headerline-buffer-status-face)))))

(defun headerline-buffer-name-c ()
  "Buffer Name and Narrow indicator"
  (mapcar #'concat
          (if headerline-buffer-id-cache-c
              (list
               (propertize (if (buffer-narrowed-p) "Narrow " "")
                           'face 'headerline-narrow-indicator-face)
               (propertize (if (car headerline-buffer-id-cache-c)
                               (car headerline-buffer-id-cache-c)
                             "")
                           'face 'headerline-buffer-parent-name-face)
               (propertize (if (car headerline-buffer-id-cache-c) "/" "")
                           'face 'headerline-buffer-file-name-face)
               (propertize (cdr headerline-buffer-id-cache-c)
                           'face 'headerline-buffer-file-name-face))
            (if buffer-file-truename
                (list
                 (propertize buffer-file-truename
                             'face 'headerline-buffer-file-name-face))
              (list "")))))

(defun headerline-major-mode-c ()
  "Major mode indicator"
  (mapcar #'concat
          (list (propertize "%[" 'face 'error)
                (propertize (format-mode-line mode-name)
                            'face 'headerline-major-mode-face)
                mode-line-process
                (propertize "%]" 'face 'error))))

(defun headerline-line-number-c ()
  (propertize "%5l" 'face 'headerline-dark-face))

(defun headerline-file-size-c ()
  (propertize "%I" 'face 'headerline-dark-face))

(defun headerline-remote-c ()
  `(:eval
    (let ((s (format-mode-line "%@")))
      (if (string-equal s "-") "" s))))

(defun headerline-mlscroll-mode-line-c ()
  "For mlscroll."
  `(:eval (mlscroll-mode-line)))

(defun headerline-macro-recording-c ()
  "Display current Emacs or evil macro being recorded."
  `(:eval
    (when (and (headerline-active)
               (or defining-kbd-macro
                   executing-kbd-macro))
      (if (bound-and-true-p evil-this-macro)
          (propertize (concat " [" (char-to-string evil-this-macro) "] ") 'face 'headerline-macro-face)
        (propertize "Macro" 'face 'headerline-macro-face)))))

(defun headerline-anzu-count-c ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for
compatibility with `evil-search'."
  `(:eval
    (when (and (bound-and-true-p anzu--state)
               (not (bound-and-true-p iedit-mode)))
      (propertize
       (let ((here anzu--current-position)
             (total anzu--total-matched))
         (cond ((eq anzu--state 'replace-query)
                (format "%d replace" anzu--cached-count))
               ((eq anzu--state 'replace)
                (format "%d/%d" (1+ here) total))
               (anzu--overflow-p
                (format "%s+" total))
               (t
                (format "%s/%d" here total))))
       'face 'headerline-match-face))))

(defun headerline-flymake-c ()
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line."
  `(:eval
    (when (and (bound-and-true-p flymake-mode)
               (flymake-running-backends))
      (list
       flymake-mode-line-exception
       (propertize
        (headerline-flymake-count-c :error)
        'face '( :weight black
                 :foreground ,headerline--err-face))
       (propertize
        (headerline-flymake-count-c :warning)
        'face '( :weight black
                 :foreground ,headerline--warn-face))
       (propertize
        (headerline-flymake-count-c :note)
        'face '( :weight black
                 :foreground ,headerline--note-face))))))

;;;;; Headerline Construction
(defun make-headerline (left)
  `(:eval
    (while-no-input
      (let* ((active? (headerline-active))
             (modified? (buffer-modified-p))
             (indicator (list
                         (if active?
                             (propertize "  " 'face 'headerline-active-indicator-face)
                           (propertize "  " 'face 'headerline-inactive-indicator-face))))
             (spacer (list " "))
             (left-list (append indicator ',left)))
        (mapcar
         #'format-mode-line
         left-list)))))

(defun set-headerline (left)
  (setq-local header-line-format (make-headerline left)))

(defun headerline-simple-mode ()
  (interactive)
  (funcall 'set-headerline
           (list
            (headerline-buffer-status-c)
            (headerline-major-mode-c) " "
            (headerline-mlscroll-mode-line-c)
            (headerline-line-number-c) " "
            (headerline-remote-c)
            (headerline-buffer-name-c) " "
            (headerline-file-size-c) " "
            (headerline-macro-recording-c)
            (headerline-anzu-count-c)
            (headerline-flymake-c)
            mode-line-misc-info)))

(add-hook 'post-command-hook #'headerline-set-active-window)

(headerline-generate-buffer-id-cache-c)

(defun mode-line-format-nil ()
  (setq mode-line-format nil))

(dolist (hook '(prog-mode-hook
                text-mode-hook
                help-mode-hook
                helpful-mode-hook
                dired-mode-hook))
  (add-hook hook 'headerline-simple-mode)
  (add-hook hook 'mode-line-format-nil))

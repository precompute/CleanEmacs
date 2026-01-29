;;; headerline-simple.el --- Simple Headerline and Modeline -*- lexical-binding: t; -*-
;;;; Header Line
;;;;; functions
(defun headerline-get-color-prop (prop face &optional fallback)
  "Get the PROP of a FACE.
If FACE is nil, set to FALLBACK."
  ((lambda (z) (if (= 35 (aref z 0)) z (apply #'color-rgb-to-hex (append (color-name-to-rgb z) (list 2)))))
   (let ((face? (face-attribute face prop)))
     (if (or (eq nil face?) (eq 'unspecified face?) (string-equal "unspecified" face?))
         (if fallback (face-attribute fallback prop) "#000000")
       face?))))

(defun mix-colors (x y &optional ratio)
  "Mix two RGB Colors `X’ and `Y’ (represented as #RRGGBB) together by `RATIO'.
Very naive mixer.  Moves towards white for ratio>=0.5 ."
  (let* ((mix
          (lambda (a b)
            (min 255
                 (truncate
                  (+ (* (or ratio 0.5) (string-to-number (substring x a b) 16))
                     (* (or ratio 0.5) (string-to-number (substring y a b) 16))))))))
    (format "#%02x%02x%02x" (funcall mix 1 3) (funcall mix 3 5) (funcall mix 5 nil))))

(defvar headerline-active-window (selected-window))

(defun headerline-set-active-window ()
  "Set hte active window in var `headerline-active-window’."
  (setq headerline-active-window (selected-window)))

(defun headerline-active ()
  "Return non-nil if the selected window has an active headerline."
  (eq (selected-window) headerline-active-window))

(defvar-local headerline-buffer-id-cache-c nil)

(defvar-local headerline-buffer-name-c-parent-name-string nil)
(defvar-local headerline-buffer-name-c-file-name-string nil)
(defconst headerline-buffer-name-c-helpful-fn-string
  (propertize "Fn " 'face 'headerline-narrow-indicator-face))
(defconst headerline-buffer-name-c-helpful-var-string
  (propertize "Var " 'face 'headerline-narrow-indicator-face))
(defun headerline-generate-buffer-id-cache-c ()
  (when after-init-time
    (let ((z
           (cond
            (buffer-file-truename
             (if-let* ((project (project-current))
                       (root (thread-last project
                                          project-root
                                          file-truename
                                          abbreviate-file-name)))
                 (cons root (substring buffer-file-truename (length root)))
               (cons nil buffer-file-truename)))
            ((eq major-mode 'dired-mode)
             (cons nil dired-directory))
            ((eq major-mode 'helpful-mode)
             (cons (if helpful--callable-p
                       headerline-buffer-name-c-helpful-fn-string
                     headerline-buffer-name-c-helpful-var-string)
                   (format "%s" helpful--sym)))
            ((or (eq major-mode 'woman-mode)
                 (eq major-mode 'Man-mode))
             (cons nil Man-page-mode-string))
            ((eq major-mode 'notmuch-search-mode)
             (cons nil notmuch-search-query-string))
            (t (cons nil (buffer-name))))))
      (setq headerline-buffer-id-cache-c
            (list
             (if (car z) (propertize (car z) 'face 'headerline-buffer-parent-name-face) "")
             (if (cdr z) (propertize (cdr z) 'face 'headerline-buffer-file-name-face) ""))))))
(dolist (hook '(change-major-mode-after-body-hook
                after-save-hook ;; In case the user saves the file to a new location
                focus-in-hook ;; ...or makes external changes then returns to Emacs
                after-set-visited-file-name-hook ;; ...when the visited file changes (e.g. it's renamed)
                after-revert-hook)) ;; ...when the underlying file changes
  (add-hook hook 'headerline-generate-buffer-id-cache-c))

(defun headerline-flymake-count-c (type)
  "Get Error/Warning/Note counts from Flycheck.
Modified from `flymake--mode-line-counter'.
TYPE can be `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (number-to-string count)))

(defvar-local headerline--flymake-cache-c nil)
(defun headerline-flymake-cache-c (&rest _)
  "Cache propertized Error/Warning/Note counts from Flycheck to `headerline--flymake-cache-c’."
  (if (and (bound-and-true-p flymake-mode)
           (fboundp 'flymake-running-backends)
           (flymake-running-backends))
      (setq headerline--flymake-cache-c
            (list flymake-mode-line-exception
                  (propertize
                   (headerline-flymake-count-c :error)
                   'face 'headerline-flymake-err-face)
                  (propertize
                   (headerline-flymake-count-c :warning)
                   'face 'headerline-flymake-warn-face)
                  (propertize
                   (headerline-flymake-count-c :note)
                   'face 'headerline-flymake-note-face)))
    (setq headerline--flymake-cache-c nil)))
(advice-add 'flymake--handle-report :after #'headerline-flymake-cache-c)
(add-hook 'flymake-mode-hook #'headerline-flymake-cache-c)

;;;;; Faces
(defface headerline-base-face
  '((t :foreground "#000"
       :background "#000"))
  "Base Face for Headerline")
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
(defface headerline-buffer-status-ED-face
  '((t :foreground "#000"))
  "Face for buffer status: EDitable.")
(defface headerline-buffer-status-RO-face
  '((t :foreground "#000"))
  "Face for buffer status: Read Only.")
(defface headerline-buffer-status-NA-face
  '((t :foreground "#000"))
  "Face for buffer status: N/A.")
(defface headerline-match-face
  '((t :foreground "#000"))
  "Face for match numbers.")
(defface headerline-macro-face
  '((t :foreground "#000"))
  "Face for indicating macros.")
(defface headerline-dark-face
  '((t :foreground "#000"))
  "Dark face.")
(defface headerline-dark-face-2
  '((t :foreground "#000"))
  "Dark face 2.")
(defface headerline-dark-face-3
  '((t :foreground "#000"))
  "Dark face 3.")
(defface headerline-flymake-err-face
  '((t :foreground "#000"))
  "Face for Flymake Err-Diagnostics.")
(defface headerline-flymake-warn-face
  '((t :foreground "#000"))
  "Face for Flymake Warn-Diagnostics.")
(defface headerline-flymake-note-face
  '((t :foreground "#000"))
  "Face for Flymake Note-Diagnostics.")

(defun set-headerline-faces (&rest rest)
  "Set headerline faces."
  (interactive)
  (let* ((fl-keyword (headerline-get-color-prop :foreground 'font-lock-keyword-face)) ;; green
         (fl-builtin (headerline-get-color-prop :foreground 'font-lock-builtin-face)) ;; blue
         (fl-constant (headerline-get-color-prop :foreground 'font-lock-constant-face)) ;; yellow
         (fl-type (headerline-get-color-prop :foreground 'font-lock-type-face)) ;; brown
         (fl-variable (headerline-get-color-prop :foreground 'font-lock-punctuation-face)) ;; off-white
         (fl-doc (headerline-get-color-prop :foreground 'font-lock-doc-face)) ;; red
         (fl-string (headerline-get-color-prop :foreground 'font-lock-string-face)) ;; red
         (region (headerline-get-color-prop :background 'region)) ;; brown
         (fl-regexp (headerline-get-color-prop :foreground 'font-lock-regexp-face 'font-lock-string-face)) ;; dark red
         (errorface (headerline-get-color-prop :foreground 'error)) ;; red
         (matchface (headerline-get-color-prop :foreground 'match)) ;; green
         ;; (warnface (headerline-get-color-prop :foreground 'font-lock-warning-face)) ;; red-ish
         (warnface (headerline-get-color-prop :foreground 'font-lock-type-face)) ;; red-ish
         (noteface (headerline-get-color-prop :background 'cursor)) ;; yellow
         (defaultfg (headerline-get-color-prop :foreground 'default)) ;; white
         (defaultbg (headerline-get-color-prop :background 'default)) ;; black
         (flymake-err-color (if errorface (mix-colors region errorface 0.45) "#000000"))
         (flymake-warn-color (if warnface (mix-colors region warnface 0.45) "#000000"))
         (flymake-note-color (if noteface (mix-colors fl-variable noteface 0.3) "#000000"))
         (theme (car custom-enabled-themes))
         (hyperstition? (memq theme '(hyperstitional-themes-rebug-flipped hyperstitional-themes-rebug)))
         (fl-variable (if hyperstition? (headerline-get-color-prop :foreground 'error) fl-variable))
         (fl-string (if hyperstition? (headerline-get-color-prop :foreground 'font-lock-builtin-face) fl-string))
         (region (if hyperstition? (headerline-get-color-prop :foreground 'menu) region))
         (height 1.3)
         (height2 0.8)
         (mix1 (mix-colors (if (not (eq 'unspecified fl-variable)) fl-variable
                             (if (not (eq 'unspecified fl-type))
                                 fl-type
                               fl-string)) region 0.6)))
    (set-face-attribute 'headerline-base-face nil
                        :inherit 'variable-pitch
                        :height height
                        :overline region
                        :underline `(:color ,fl-variable :position 0)
                        :foreground fl-variable
                        :background region)
    (set-face-attribute 'headerline-file-modified-face nil
                        :foreground fl-builtin)
    (set-face-attribute 'headerline-file-unmodified-face nil
                        :foreground fl-keyword)
    (set-face-attribute 'headerline-narrow-indicator-face nil
                        :foreground fl-regexp
                        :weight 'bold)
    (set-face-attribute 'headerline-buffer-parent-name-face nil
                        :foreground mix1
                        :weight 'bold)
    (set-face-attribute 'headerline-buffer-file-name-face nil
                        :weight 'normal
                        :foreground mix1)
    (set-face-attribute 'headerline-major-mode-face nil
                        :foreground mix1
                        :weight 'bold)
    (set-face-attribute 'headerline-buffer-status-ED-face nil
                        :inherit 'fixed-pitch
                        :foreground (cl-reduce #'mix-colors (list region fl-keyword fl-constant))
                        :background (cl-reduce #'mix-colors (list region fl-keyword fl-constant)))
    (set-face-attribute 'headerline-buffer-status-RO-face nil
                        :inherit 'fixed-pitch
                        :foreground (cl-reduce #'mix-colors (list region (if (not (eq 'unspecified fl-doc)) fl-doc errorface) defaultbg))
                        :background (cl-reduce #'mix-colors (list region (if (not (eq 'unspecified fl-doc)) fl-doc errorface) defaultbg)))
    (set-face-attribute 'headerline-buffer-status-NA-face nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'headerline-match-face nil
                        :foreground fl-keyword
                        :weight 'bold)
    (set-face-attribute 'headerline-macro-face nil
                        :foreground fl-constant)
    (set-face-attribute 'headerline-dark-face nil
                        :inherit 'fixed-pitch-numbers
                        :foreground (mix-colors region fl-string 0.9)
                        :weight 'bold)
    (set-face-attribute 'headerline-dark-face-2 nil
                        :inherit 'fixed-pitch-numbers
                        :foreground (mix-colors region fl-builtin 0.9)
                        :weight 'bold)
    (set-face-attribute 'headerline-dark-face-3 nil
                        :inherit 'fixed-pitch-numbers
                        :foreground (mix-colors region fl-keyword 0.6)
                        :weight 'bold)
    (set-face-attribute 'header-line-inactive nil
                        :height height
                        :inherit 'variable-pitch
                        :foreground (mix-colors fl-variable defaultbg 0.2)
                        :background (mix-colors region defaultbg 0.45)
                        :overline (mix-colors region defaultbg 0.45))
    (set-face-attribute 'mode-line nil
                        :background defaultbg
                        :box nil
                        :overline fl-variable)
    (set-face-attribute 'mode-line-inactive nil
                        :background defaultbg
                        :box nil
                        :overline nil)
    (set-face-attribute 'headerline-flymake-err-face nil
                        :weight 'black
                        :inherit 'fixed-pitch-numbers
                        :foreground flymake-err-color)
    (set-face-attribute 'headerline-flymake-warn-face nil
                        :weight 'black
                        :inherit 'fixed-pitch-numbers
                        :foreground flymake-warn-color)
    (set-face-attribute 'headerline-flymake-note-face nil
                        :weight 'black
                        :inherit 'fixed-pitch-numbers
                        :foreground flymake-note-color)
    ;; (defvar headerline--default-face (if defaultfg (mix-colors region defaultfg) "#000000"))
    (if (and (fboundp 'mlscroll-mode) (mlscroll-mode) (boundp 'mlscroll-in-color) (boundp 'mlscroll-out-color))
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
(defconst headerline-buffer-status-c--read-only-string
  (propertize "  " 'face 'headerline-buffer-status-RO-face))
(defconst headerline-buffer-status-c--modified-p-string
  (propertize "  " 'face 'headerline-buffer-status-ED-face))
(defconst headerline-buffer-status-c--default-string
  (propertize "  " 'face 'headerline-buffer-status-NA-face))
(defun headerline-buffer-status-c ()
  "Buffer RO/modified/none"
  (list " "
        (cond (buffer-read-only headerline-buffer-status-c--read-only-string)
              ((buffer-modified-p) headerline-buffer-status-c--modified-p-string)
              (t headerline-buffer-status-c--default-string))
        " "))

(defun headerline-buffer-status-with-evil-c ()
  "Buffer RO/modified/none & evil state."
  `(:eval
    (let ((z (if evil-local-mode
                 (cadr (assoc evil-state (list (list 'normal "N")
                                               (list 'insert "I")
                                               (list 'visual "V")
                                               (list 'replace "R"))))
               "  ")))
      (list
       " "
       (cond (buffer-read-only (propertize z 'face 'headerline-buffer-status-RO-face))
             ((buffer-modified-p) (propertize z 'face 'headerline-buffer-status-ED-face))
             (t (propertize z 'face 'headerline-buffer-status-NA-face)))
       " "))))

(defun headerline-buffer-name-c ()
  "Buffer Name and Narrow indicator"
  headerline-buffer-id-cache-c)

(defun headerline-help-echo-c (window object pos)
  "Help Echo string for the buffer-name component of WINDOW.
OBJECT and POS are ignored."
  (with-selected-window window
    (format "%s" (or (car headerline-buffer-id-cache-c) buffer-file-truename helpful--sym))))

(defconst headerline-major-mode-c-rec-edit-start-string (propertize "%[" 'face 'error))
(defconst headerline-major-mode-c-rec-edit-end-string (propertize "%]" 'face 'error))
(defun headerline-major-mode-c ()
  "Major mode indicator"
  (list headerline-major-mode-c-rec-edit-start-string
        '(:propertize mode-name face headerline-major-mode-face)
        mode-line-process
        headerline-major-mode-c-rec-edit-end-string))

(defconst headerline-line-number-c (propertize "%5l" 'face 'headerline-dark-face))

(defconst headerline-file-size-c (propertize "%I" 'face 'headerline-dark-face))

(defconst headerline-str-remote (propertize "@" 'face 'headerline-dark-face))
(defun headerline-remote-c ()
  "Remote buffer check."
  (when (file-remote-p default-directory) headerline-str-remote))

(defun headerline-mlscroll-mode-line-c ()
  "For mlscroll."
  `(:eval (mlscroll-mode-line)))

(defconst headerline-buffer-percent-c
  (propertize "%P" 'face 'headerline-dark-face-2)
  "Buffer percent / Bot / Top / All.")

(defconst headerline-buffer-name-c-narrow-string
  (propertize "Narrow " 'face 'headerline-narrow-indicator-face))
(defun headerline-narrow-c ()
  "Narrow."
  (if (buffer-narrowed-p) headerline-buffer-name-c-narrow-string ""))

(defun headerline-macro-recording-c ()
  "Display current Emacs or evil macro being recorded."
  (when (and (headerline-active)
             (or defining-kbd-macro
                 executing-kbd-macro))
    (if (bound-and-true-p evil-this-macro)
        (propertize (concat " [" (char-to-string evil-this-macro) "] ") 'face 'headerline-macro-face)
      (propertize " Macro " 'face 'headerline-macro-face))))

(defun headerline-anzu-count-c ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for
compatibility with `evil-search'."
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format "%d replace " anzu--cached-count))
             ((eq anzu--state 'replace)
              (format "%d/%d " (1+ here) total))
             (anzu--overflow-p
              (format "%s+ " total))
             (t
              (format "%s/%d " here total))))
     'face 'headerline-dark-face-3)))

(defun headerline-flymake-c ()
  "Return `headerline--flymake-cache-c’."
  headerline--flymake-cache-c)

(defun headerline-breadcrumb-c ()
  "Construct that displays `breadcrumb-imenu-crumbs’.
Specific to the current window."
  `(:eval (breadcrumb-imenu-crumbs)))

(defun headerline-evil-state-c ()
  "Display the current evil state if evil-mode is active locally."
  `(:eval
    (if evil-local-mode
        (propertize
         (cadr (assoc evil-state (list (list 'normal " N ")
                                       (list 'insert " I ")
                                       (list 'visual " V ")
                                       (list 'replace " R "))))
         'face '(:inherit fixed-pitch :height 0.8))
      "")))

(defun headerline-right-align-c ()
  "Wrapper around the builtin function `mode--line-format-right-align’.
Functionally equivalent to `mode-line-format-right-align’."
  `(:eval (mode--line-format-right-align)))

;;;;; Headerline Construction
(defun headerline-simple-mode ()
  (interactive)
  (setq-local header-line-format `(:eval (list (headerline-buffer-status-c)
                                               (headerline-major-mode-c) " "
                                               headerline-line-number-c " "
                                               ;; (headerline-mlscroll-mode-line-c)
                                               headerline-buffer-percent-c " "
                                               (headerline-remote-c)
                                               (headerline-narrow-c) " "
                                               (headerline-buffer-name-c) " "
                                               ;; headerline-file-size-c " "
                                               (headerline-macro-recording-c)
                                               (headerline-anzu-count-c)
                                               (headerline-flymake-c)
                                               mode-line-misc-info))))

(add-hook 'post-command-hook #'headerline-set-active-window)

(headerline-generate-buffer-id-cache-c)

(defun modeline-simple-mode ()
  (interactive)
  (if (and (fboundp 'breadcrumb--header-line)
           (boundp 'enable-breadcrumb?)
           (or (buffer-local-value 'enable-breadcrumb? (current-buffer))
               (eq major-mode 'org-mode)))
      (setq-local ;; mode-line-right-align-edge 'left-fringe
       mode-line-format
       `(:eval (list (headerline-breadcrumb-c))))
    (setq mode-line-format nil)))

(dolist (hook '(prog-mode-hook
                text-mode-hook
                help-mode-hook
                helpful-mode-hook
                messages-buffer-mode-hook
                fundamental-mode-hook
                conf-mode-hook
                conf-space-mode-hook
                doc-view-mode-hook
                dired-mode-hook
                apropos-mode-hook
                debugger-mode-hook
                messages-buffer-mode-hook
                shortdoc-mode-hook
                finder-mode-hook
                image-mode-hook
                magit-status-mode-hook
                man-common-hook
                vterm-mode-hook
                xref--xref-buffer-mode-hook
                xref--transient-buffer-mode-hook
                notmuch-hello-mode-hook
                notmuch-search-mode-hook))
  (add-hook hook 'headerline-simple-mode 100)
  (add-hook hook 'modeline-simple-mode 100))

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
  "Generate the Buffer ID (base dir / filename) cache."
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
    (format " %s " count)))

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
(defface headerline-base-face '((t)) "Base Face for Headerline.")
(defface headerline-file-modified-face '((t)) "File Modified Face for Headerline.")
(defface headerline-file-unmodified-face '((t)) "File Unmodified Face for Headerline.")
(defface headerline-narrow-indicator-face '((t)) "Face for narrowing.")
(defface headerline-buffer-parent-name-face '((t)) "Face for parent in buffer name.")
(defface headerline-buffer-file-name-face '((t)) "Face for file in buffer name.")
(defface headerline-major-mode-face '((t)) "Face for major mode.")
(defface headerline-buffer-status-ED-face '((t)) "Face for buffer status: EDitable.")
(defface headerline-buffer-status-RO-face '((t)) "Face for buffer status: Read Only.")
(defface headerline-buffer-status-NA-face '((t)) "Face for buffer status: N/A.")
(defface headerline-match-face '((t)) "Face for match numbers.")
(defface headerline-macro-face '((t)) "Face for indicating macros.")
(defface headerline-dark-face '((t)) "Dark face.")
(defface headerline-dark-face-2 '((t)) "Dark face 2.")
(defface headerline-dark-face-3 '((t)) "Dark face 3.")
(defface headerline-flymake-err-face '((t)) "Face for Flymake Err-Diagnostics.")
(defface headerline-flymake-warn-face '((t)) "Face for Flymake Warn-Diagnostics.")
(defface headerline-flymake-note-face '((t)) "Face for Flymake Note-Diagnostics.")
(defface headerline-custom-space-face '((t)) "Face for spaces.")
(defface headerline-evil-insert-active-face '((t)) "Face for when evil insert is active.")

(unless (facep 'fixed-pitch-numbers) (copy-face 'fixed-pitch 'fixed-pitch-numbers))

(defun set-headerline-faces (&optional theme)
  "Set headerline faces for THEME."
  (interactive)
  (let* ((fl-keyword (headerline-get-color-prop :foreground 'font-lock-keyword-face)) ;; green
         (fl-builtin (headerline-get-color-prop :foreground 'font-lock-builtin-face)) ;; blue
         (fl-constant (headerline-get-color-prop :foreground 'font-lock-constant-face)) ;; yellow
         (fl-type (headerline-get-color-prop :foreground 'font-lock-type-face)) ;; brown
         (fl-variable (headerline-get-color-prop :foreground 'font-lock-punctuation-face)) ;; off-white
         (fl-doc (headerline-get-color-prop :foreground 'font-lock-doc-face)) ;; red
         (fl-string (headerline-get-color-prop :foreground 'font-lock-string-face)) ;; red
         (regionface (headerline-get-color-prop :background 'region)) ;; brown
         (fl-regexp (headerline-get-color-prop :foreground 'font-lock-regexp-face 'font-lock-string-face)) ;; dark red
         (successface (headerline-get-color-prop :foreground 'success))
         (errorface (headerline-get-color-prop :foreground 'error)) ;; red
         (matchface (headerline-get-color-prop :foreground 'match)) ;; green
         ;; (warnface (headerline-get-color-prop :foreground 'font-lock-warning-face)) ;; red-ish
         (warnface (headerline-get-color-prop :foreground 'font-lock-type-face)) ;; red-ish
         (noteface (headerline-get-color-prop :background 'cursor)) ;; yellow
         (defaultfg (headerline-get-color-prop :foreground 'default)) ;; white
         (defaultbg (headerline-get-color-prop :background 'default)) ;; black
         (flymake-err-color (if errorface (mix-colors regionface errorface 0.45) "#000000"))
         (flymake-warn-color (if warnface (mix-colors regionface warnface 0.45) "#000000"))
         (flymake-note-color (if noteface (mix-colors fl-variable noteface 0.3) "#000000"))
         (theme (car custom-enabled-themes))
         (light-theme? (let ((z (or theme (car custom-enabled-themes))))
                         (eq 'light (plist-get (get z 'theme-properties) :background-mode))))
         (hyperstition? (memq theme '(hyperstitional-themes-rebug-flipped hyperstitional-themes-rebug)))
         (fl-variable (if hyperstition? (headerline-get-color-prop :foreground 'error) fl-variable))
         (fl-string (if hyperstition? (headerline-get-color-prop :foreground 'font-lock-builtin-face) fl-string))
         (regionface (if hyperstition? (headerline-get-color-prop :foreground 'menu) regionface))
         (height 1.3)
         (height2 1.0)
         (mix1 (mix-colors (if (not (eq 'unspecified fl-variable)) fl-variable
                             (if (not (eq 'unspecified fl-type))
                                 fl-type
                               fl-string)) regionface 0.6)))
    (let ((c (mix-colors regionface (if light-theme? defaultfg fl-doc) (if light-theme? 0.75 0.25))))
      (set-face-attribute 'headerline-base-face nil
                          :inherit 'variable-pitch
                          :height height
                          :weight 'normal
                          :foreground fl-variable
                          :background c
                          :box `(:color ,c :line-width (-1 . 4))))
    (set-face-attribute 'header-line-inactive nil
                        :inherit 'variable-pitch
                        :height height
                        :weight 'normal
                        :foreground (mix-colors defaultbg defaultfg 0.2)
                        :background defaultbg
                        :box `(:color ,defaultbg :line-width (-1 . 4)))
    (set-face-attribute 'headerline-custom-space-face nil
                        :inherit 'variable-pitch
                        :weight 'normal)
    (set-face-attribute 'headerline-file-modified-face nil
                        :foreground fl-builtin)
    (set-face-attribute 'headerline-file-unmodified-face nil
                        :foreground fl-keyword)
    (set-face-attribute 'headerline-narrow-indicator-face nil
                        :foreground fl-regexp
                        :weight 'bold)
    (set-face-attribute 'headerline-buffer-parent-name-face nil
                        :foreground fl-builtin
                        :weight 'bold)
    (set-face-attribute 'headerline-buffer-file-name-face nil
                        :foreground fl-keyword
                        :weight 'normal)
    (set-face-attribute 'headerline-major-mode-face nil
                        :foreground (mix-colors mix1 fl-constant 0.75)
                        :weight 'bold)
    (set-face-attribute 'headerline-buffer-status-ED-face nil
                        ;; :foreground (cl-reduce #'mix-colors (list regionface fl-keyword fl-constant)))
                        :foreground successface)
    (set-face-attribute 'headerline-buffer-status-RO-face nil
                        ;; :foreground (cl-reduce #'mix-colors (list regionface (if (not (eq 'unspecified fl-doc)) fl-doc errorface) defaultbg))
                        :foreground errorface)
    (set-face-attribute 'headerline-buffer-status-NA-face nil
                        :foreground (mix-colors mix1 fl-constant 0.75)
                        :background 'unspecified
                        :inherit nil)
    (set-face-attribute 'headerline-match-face nil
                        :foreground fl-keyword
                        :weight 'bold)
    (set-face-attribute 'headerline-macro-face nil
                        :foreground fl-constant)
    (set-face-attribute 'headerline-dark-face nil
                        :inherit 'fixed-pitch-numbers
                        :inverse-video nil
                        :weight 'bold
                        :height 1.25
                        :foreground (cl-reduce #'mix-colors (list regionface fl-string defaultfg)))
    (set-face-attribute 'headerline-dark-face-2 nil
                        :inherit 'fixed-pitch-numbers
                        :inverse-video nil
                        :weight 'bold
                        :height 1.25
                        :foreground (cl-reduce #'mix-colors (list regionface fl-builtin defaultfg)))
    (set-face-attribute 'headerline-dark-face-3 nil
                        :inherit 'fixed-pitch-numbers
                        :inverse-video nil
                        :weight 'bold
                        :height 1.25
                        :foreground (cl-reduce #'mix-colors (list regionface fl-keyword defaultfg)))
    (set-face-attribute 'mode-line nil
                        :inherit 'fixed-pitch-numbers
                        :background 'unspecified
                        :box nil
                        :overline nil)
    (set-face-attribute 'mode-line-inactive nil
                        :inherit 'fixed-pitch-numbers
                        :background 'unspecified
                        :box nil
                        :overline nil)
    (set-face-attribute 'headerline-flymake-err-face nil
                        :inherit 'fixed-pitch-numbers
                        :inverse-video t
                        :weight 'bold
                        :height 1.25
                        :foreground flymake-err-color
                        :box `(:color ,flymake-err-color :line-width (1 . -1))) ;; box fills the entire height automatically
    (set-face-attribute 'headerline-flymake-warn-face nil
                        :inherit 'fixed-pitch-numbers
                        :inverse-video t
                        :weight 'bold
                        :height 1.25
                        :foreground flymake-warn-color
                        :box `(:color ,flymake-warn-color :line-width (1 . -1)))
    (set-face-attribute 'headerline-flymake-note-face nil
                        :inherit 'fixed-pitch-numbers
                        :inverse-video t
                        :weight 'bold
                        :height 1.25
                        :foreground flymake-note-color
                        :box `(:color ,flymake-note-color :line-width (1 . -1)))
    (set-face-attribute 'headerline-evil-insert-active-face nil
                        :foreground (mix-colors fl-keyword fl-builtin)
                        :background 'unspecified)
    ;; (defvar headerline--default-face (if defaultfg (mix-colors regionface defaultfg) "#000000"))
    (if (and (fboundp 'mlscroll-mode) (mlscroll-mode) (boundp 'mlscroll-in-color) (boundp 'mlscroll-out-color))
        (progn (setq-default mlscroll-in-color
                             (cl-reduce #'mix-colors (list regionface (if (not (eq 'unspecified fl-doc)) fl-doc errorface) fl-keyword)))
               (setq-default mlscroll-out-color defaultbg)
               (mlscroll-mode -1) (mlscroll-mode 1)))
    (update-face-remapping-alist 'headerline-base-face 'header-line)))

(defun update-face-remapping-alist (face target)
  "Update `face-remapping-alist’ for FACE in TARGET."
  (unless (member face face-remapping-alist)
    (push (list target face) face-remapping-alist)))

(set-headerline-faces) ;; init during load
(add-to-list 'enable-theme-functions 'set-headerline-faces)
(if (fboundp 'set-fonts-c) (add-to-list 'enable-theme-functions 'set-fonts-c))

;;;;; Headerline Constructs
(defconst headerline-custom-space (propertize " " 'face 'headerline-custom-space-face)) ;;haha
(defconst headerline-custom-space-2 (propertize "  " 'face 'headerline-custom-space-face))
(defconst headerline-custom-space-3 (propertize "   " 'face 'headerline-custom-space-face))

(defconst headerline-buffer-status-c--read-only-string
  (propertize " " 'display (create-image
                            (expand-file-name ".images/modeline/juliamonocomma.svg" user-emacs-directory)
                            'svg nil :ascent 'center :width 15)
              'face 'headerline-buffer-status-RO-face))
(defconst headerline-buffer-status-c--modified-p-string
  (propertize " " 'display (create-image
                            (expand-file-name ".images/modeline/juliamonoplusrot1.svg" user-emacs-directory)
                            'svg nil :ascent 'center :width 15)
              'face 'headerline-buffer-status-ED-face))
(defconst headerline-buffer-status-c--modified-p-string-2
  (propertize " " 'display (create-image
                            (expand-file-name ".images/modeline/juliamonoplusrot2.svg" user-emacs-directory)
                            'svg nil :ascent 'center :width 15)
              'face 'headerline-buffer-status-ED-face))
(defconst headerline-buffer-status-c--default-string
  (propertize " " 'display (create-image
                            (expand-file-name ".images/modeline/juliamonoasterisk.svg" user-emacs-directory)
                            'svg nil :ascent 'center :width 15)
              'face 'headerline-buffer-status-NA-face))
(defconst headerline-buffer-status-c--default-string-2
  (propertize " " 'display (create-image
                            (expand-file-name ".images/modeline/juliamonoasterisk.svg" user-emacs-directory)
                            'svg nil :ascent 'center :width 15 :rotation 90)
              'face 'headerline-buffer-status-NA-face))
(defun headerline-buffer-status-c ()
  "Buffer RO/modified/none."
  (cond (buffer-read-only headerline-buffer-status-c--read-only-string)
        ((buffer-modified-p)
         (if (evenp (point))
             headerline-buffer-status-c--modified-p-string
           headerline-buffer-status-c--modified-p-string-2))
        (t (if (evenp (line-number-at-pos))
               headerline-buffer-status-c--default-string
             headerline-buffer-status-c--default-string-2))))
;; We use `line-number-at-pos’ and not `point’ because in normal-mode the
;; modeline seemingly only updates when the line number changes.  Maybe
;; this is an optimization in Emacs.  Should I add an invisible
;; point-construct to the modeline to force another optimization?

(defconst headerline-evil-insert-state-indicator-c--inactive-string
  (propertize " " 'display (create-image
                            (expand-file-name ".images/modeline/blanksquare.svg" user-emacs-directory)
                            'svg nil :ascent 'center :width 5)))
(defconst headerline-evil-insert-state-indicator-c--active-string
  (propertize " " 'display (create-image
                            (expand-file-name ".images/modeline/juliamonoperiod.svg" user-emacs-directory)
                            'svg nil :ascent 'center :width 5)
              'face 'headerline-evil-insert-active-face))
(defvar-local headerline-evil-insert-state-indicator-var-c
  headerline-evil-insert-state-indicator-c--inactive-string)
(defun headerline-evil-insert-state-indicator-c--enable ()
  "Set the insert state variable to the active-string."
  (setq-local headerline-evil-insert-state-indicator-var-c
              headerline-evil-insert-state-indicator-c--active-string))
(defun headerline-evil-insert-state-indicator-c--disable ()
  "Set the insert state variable to the inactive-string."
  (setq-local headerline-evil-insert-state-indicator-var-c
              headerline-evil-insert-state-indicator-c--inactive-string))
(add-hook 'evil-insert-state-exit-hook #'headerline-evil-insert-state-indicator-c--disable)
(add-hook 'evil-insert-state-entry-hook #'headerline-evil-insert-state-indicator-c--enable)
(defun headerline-evil-insert-state-indicator-c ()
  "Display evil-mode’s insert state."
  headerline-evil-insert-state-indicator-var-c)

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
  "Buffer Name and Narrow indicator."
  headerline-buffer-id-cache-c)

(defun headerline-help-echo-c (window object pos)
  "Help Echo string for the buffer-name component of WINDOW.
OBJECT and POS are ignored."
  (with-selected-window window
    (format "%s" (or (car headerline-buffer-id-cache-c) buffer-file-truename helpful--sym))))

(defconst headerline-major-mode-c-rec-edit-start-string (propertize "%[" 'face 'error))
(defconst headerline-major-mode-c-rec-edit-end-string (propertize "%]" 'face 'error))
(defun headerline-major-mode-c ()
  "Major mode indicator."
  (list headerline-major-mode-c-rec-edit-start-string
        '(:propertize mode-name face headerline-major-mode-face)
        mode-line-process
        headerline-major-mode-c-rec-edit-end-string))

(defconst headerline-line-number-c (propertize "%4l" 'face 'headerline-dark-face))

(defconst headerline-file-size-c (propertize "%I" 'face 'headerline-dark-face))

(defconst headerline-str-remote (propertize "@" 'face 'headerline-dark-face))
(defun headerline-remote-c ()
  "Remote buffer check."
  (when (file-remote-p default-directory) headerline-str-remote))

(defun headerline-mlscroll-mode-line-c ()
  "For mlscroll."
  `(:eval (mlscroll-mode-line)))

(setq mode-line-percent-position '(-3 "%o"))
(defconst headerline-buffer-percent-c
  (propertize "%o" 'face 'headerline-dark-face-2)
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
  "Display the current evil state if `evil-mode’ is active locally."
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
  "Custom Header-Line mode.  Changes `header-line-format’ for the current buffer (with `setq-local’)."
  (interactive)
  (setq-local header-line-format `(:eval (list headerline-custom-space-3
                                               (headerline-buffer-status-c)
                                               (headerline-evil-insert-state-indicator-c)
                                               headerline-custom-space
                                               (headerline-major-mode-c)
                                               headerline-custom-space-2
                                               ;; (headerline-mlscroll-mode-line-c)
                                               (headerline-remote-c)
                                               (headerline-narrow-c)
                                               headerline-custom-space
                                               (headerline-buffer-name-c)
                                               headerline-custom-space-2
                                               (headerline-macro-recording-c)
                                               (headerline-anzu-count-c)
                                               (headerline-flymake-c)
                                               headerline-custom-space
                                               headerline-line-number-c
                                               headerline-custom-space
                                               headerline-buffer-percent-c
                                               headerline-custom-space
                                               mode-line-misc-info))))

(add-hook 'post-command-hook #'headerline-set-active-window)

(headerline-generate-buffer-id-cache-c)

(defun modeline-simple-mode ()
  "Custom Mode-Line mode.  Changes `mode-line-format’ for the current buffer (with `setq-local’)."
  (interactive)
  (if (and (fboundp 'breadcrumb--header-line)
           (boundp 'enable-breadcrumb?)
           (or (buffer-local-value 'enable-breadcrumb? (current-buffer))
               (eq major-mode 'org-mode)))
      (setq-local ;; mode-line-right-align-edge 'left-fringe
       mode-line-format
       `(:eval (list headerline-custom-space-2
                     (headerline-breadcrumb-c))))
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
                sparro-append-chat-mode-hook
                notmuch-search-mode-hook))
  (add-hook hook 'headerline-simple-mode 100)
  (add-hook hook 'modeline-simple-mode 100))

;; [26-01-30 02:49:38] I’ve put in too much time trying to make the
;; headerline and modeline right-aligned.  The fault seems to lie with
;; variable-pitch faces; first from sculpture-theme and second from
;; headerline-simple.el.  However, it could be because I cache variables;
;; or maybe because I do `setq-local’ via a hook instead of `setq-default’
;; for mode-line-format and header-line-format.
;; I tried setting mode-line-right-align-edge to all three values and
;; tested every possible combination twice.  The following works perfectly,
;; occasionally even with variable-pitch faces:
;; (setq-default mode-line-format `("%f %l %o" mode-line-format-right-align "aaa %+ %q z"))
;; (setq-default header-line-format `("%f %l %o" mode-line-format-right-align "aaa %+ %q z"))

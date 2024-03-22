;;; Header Line
;;;; Variables
(defvar modeline-active-window (selected-window))

(defvar modeline-evil-state-cached ""
  "Cached `evil-state’ for display in modeline.")

;;;; functions
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

(defun modeline-active ()
  "Return non-nil if the selected window has an active modeline."
  (eq (selected-window) modeline-active-window))

(defun modeline-inactive ()
  "Return non-nil if the selected window has an inactive modeline."
  (not (modeline-active)))

;; from Doom-modeline
(defun modeline-set-selected-window-c (&rest _)
  "Track the active modeline's window in `modeline-active-window'."
  (let ((win (selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq modeline-active-window (frame-selected-window)))))
(add-hook 'pre-redisplay-functions 'modeline-set-selected-window-c)

(defvar-local modeline-buffer-id-cache-c nil)

(defun modeline-generate-buffer-id-cache-c ()
  (when after-init-time
    (setq modeline-buffer-id-cache-c
          (if buffer-file-truename
              (let ((g-root (locate-dominating-file buffer-file-truename ".git")))
                (if (not g-root)
                    nil
                  (let* ((buf (file-truename buffer-file-truename))
                         (gitroot (file-truename g-root))
                         (gitroot-nodash (substring gitroot 0 -1))
                         (gitroot-a (replace-regexp-in-string
                                     (rx (seq (* anychar) "/"))
                                     "" gitroot-nodash))
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
              nil)))))
(dolist (hook '(change-major-mode-after-body-hook
                after-save-hook ;; In case the user saves the file to a new location
                focus-in-hook ;; ...or makes external changes then returns to Emacs
                projectile-after-switch-project-hook ;; ...or when we change the current project!
                after-set-visited-file-name-hook ;; ...when the visited file changes (e.g. it's renamed)
                after-revert-hook)) ;; ...when the underlying file changes
  (add-hook hook 'modeline-generate-buffer-id-cache-c))

(defun colortovar-c (var color)
  (if color
      (defvar var color)
    (defvar var "#000000")))

(defun headerline-flymake-count-c (type)
  "Get Error/Warning/Note counts from Flycheck.
Modified from `flymake--mode-line-counter'.
`type' can be `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (or (cl-plusp count)
              (cond ((eq flymake-suppress-zero-counters t)
                     nil)
                    (flymake-suppress-zero-counters
                     (>= (flymake--severity type)
                         (warning-numeric-level
                          flymake-suppress-zero-counters)))
                    (t t))))
    (number-to-string count)))

(defun modeline-evil-state-set-cache ()
  "Cached value for Evil State for the modeline."
  (interactive)
  (let ((normal-state-tag "N")
        (insert-state-tag "I")
        (visual-state-tag "V")
        (visual-char-state-tag "VC")
        (visual-line-state-tag "VL")
        (visual-screen-line-state-tag "VSL")
        (visual-block-state-tag "VB")
        (motion-state-tag "M")
        (replace-state-tag "R")
        (operator-state-tag "O")
        (emacs-state-tag "E")
        (e evil-state))
    (setq-local modeline-evil-state-cached
                (if (not (string-equal e "visual"))
                    (symbol-value (intern (format "%s-state-tag" e)))
                  (symbol-value (intern (format "visual-%s-state-tag" evil-visual-selection)))))))

;; (dolist (h '(evil-emacs-state-entry-hook
;;              evil-insert-state-entry-hook
;;              evil-motion-state-entry-hook
;;              evil-normal-state-entry-hook
;;              evil-visual-state-entry-hook
;;              evil-replace-state-entry-hook
;;              evil-operator-state-entry-hook))
;;   (add-hook h 'modeline-evil-state-set-cache))

;;;; Faces
(defface headerline-modified-active
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width (1 . 3) :color "#000")))
  "Headerline when buffer is modified and inactive.")
(defface headerline-unmodified-active
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width (1 . 3) :color "#000")))
  "Headerline when buffer is unmodified and inactive.")
(defface headerline-active-indicator
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for active window.")
(defface headerline-inactive-modified-indicator
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for inactive, modified window.")
(defface headerline-inactive-unmodified-indicator
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for inactive, unmodified window.")
(defface headerline-narrow-indicator
  '((t :foreground "#000"))
  "Indicator for narrowing.")

(defun set-headerline-faces (&rest rest)
  (interactive)
  (let ((fl-keyword (get-color-fg 'font-lock-keyword-face)) ;; green
        (fl-builtin (get-color-fg 'font-lock-builtin-face)) ;; blue
        (fl-type (get-color-fg 'font-lock-type-face)) ;; brown
        (fl-variable (get-color-fg 'font-lock-punctuation-face)) ;; off-white
        (fl-regexp (get-color-fg 'font-lock-regexp-face 'font-lock-string-face)) ;; dark red
        (errorface (get-color-fg 'error)) ;; red
        (matchface (get-color-fg 'match)) ;; green
        ;; (warnface (get-color-fg 'font-lock-warning-face)) ;; red-ish
        (warnface (get-color-fg 'font-lock-type-face)) ;; red-ish
        (noteface (get-color-bg 'hi-yellow)) ;; yellow
        (defaultfg (get-color-fg 'default)) ;; white
        (defaultbg (get-color-bg 'default))) ;; black
    (set-face-attribute 'headerline-modified-active nil
                        :foreground fl-variable
                        ;; :foreground defaultbg
                        :background fl-builtin
                        :box (list :line-width '(1 . 3) :color fl-builtin))
    (set-face-attribute 'headerline-unmodified-active nil
                        ;; :foreground fl-variable
                        :foreground defaultbg
                        :background fl-keyword
                        :box (list :line-width '(1 . 3) :color fl-keyword))
    (set-face-attribute 'headerline-active-indicator nil
                        :height 1.4
                        :foreground defaultfg
                        :background defaultfg)
    (set-face-attribute 'headerline-inactive-modified-indicator nil
                        :height 1.4
                        :foreground fl-builtin
                        :background fl-builtin)
    (set-face-attribute 'headerline-inactive-unmodified-indicator nil
                        :height 1.4
                        :foreground fl-keyword
                        :background fl-keyword)
    (set-face-attribute 'headerline-narrow-indicator nil
                        :height 1.4
                        :foreground fl-regexp
                        :weight 'bold
                        :inherit 'variable-pitch)
    (defvar headerline--err-face (if errorface errorface "#000000"))
    (defvar headerline--warn-face (if warnface warnface "#000000"))
    (defvar headerline--note-face (if noteface noteface "#000000"))
    (defvar headerline--default-face (if defaultfg defaultfg "#000000"))
    (defvar headerline--evil-state-face fl-regexp)
    (if (and (mlscroll-mode) (boundp 'mlscroll-in-color) (boundp 'mlscroll-out-color))
        (progn
          (setq-default mlscroll-in-color fl-regexp)
          (setq-default mlscroll-out-color defaultbg)
          (mlscroll-mode -1) (mlscroll-mode 1)))))
(set-headerline-faces) ;; init during load
(add-to-list 'enable-theme-functions 'set-headerline-faces)
(if (fboundp 'set-fonts-c) (add-to-list 'enable-theme-functions 'set-fonts-c))

(defun update-face-remapping-alist (face target)
  (if (member face face-remapping-alist)
      (face-remap-reset-base target)
    (push (list target face) face-remapping-alist)))

;;;; Modeline Constructs
;; Referenced from https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/

(defvar modeline-bg-color-change-c
  '(:eval
    (if (modeline-active)
        (if (buffer-modified-p)
            (progn
              (update-face-remapping-alist 'headerline-modified-active 'header-line)
              "")
          (update-face-remapping-alist 'headerline-unmodified-active 'header-line)
          "")))
  "Change the background color of the modeline.")

(defvar modeline-active-indicator-c
  '(:eval
    (let ((text "  "))
      (if (modeline-active)
          (propertize text
                      'face 'headerline-active-indicator)
        (if (buffer-modified-p)
            (propertize text
                        'face 'headerline-inactive-modified-indicator)
          (propertize text
                      'face 'headerline-inactive-unmodified-indicator))))))

(defvar modeline-active-indicator-with-state-c
  '(:eval
    (let ((text (concat " " modeline-evil-state-cached " ")))
      (if (modeline-active)
          (propertize text
                      'face 'headerline-active-indicator)
        (if (buffer-modified-p)
            (propertize text
                        'face 'headerline-inactive-modified-indicator)
          (propertize text
                      'face 'headerline-inactive-unmodified-indicator))))))

(defvar modeline-modes-c
  (list (propertize "%[" 'face 'error)
        `(:propertize (concat "" mode-name)
                      face (:inherit variable-pitch :height 1.4 :weight black))
        mode-line-process
        (propertize "%]" 'face 'error)
        " ")
  "Mode line construct for displaying major modes.")

(defvar modeline-current-buffer-property-c
  '(:eval
    (if buffer-read-only
        (propertize " RO" 'face '(:inherit error :height 1.2 :weight bold))))
  "Show read-only status")

(defvar modeline-current-buffer-narrowed-c
  '(:propertize "%n " face headerline-narrow-indicator)
  "Show narrowing status")

(defvar modeline-current-buffer-name-c
  '(:eval
    (if modeline-buffer-id-cache-c
        (list
         (propertize (if (car modeline-buffer-id-cache-c)
                         (car modeline-buffer-id-cache-c)
                       "")
                     'face '( :inherit variable-pitch
                              :weight bold
                              :height 1.15))
         (propertize (if (car modeline-buffer-id-cache-c) "/" "")
                     'face '( :inherit variable-pitch
                              :height 1.15))
         (propertize
          (cdr modeline-buffer-id-cache-c)
          'face '( :inherit variable-pitch
                   :height 1.15)))
      (if buffer-file-truename
          (propertize buffer-file-truename
                      'face '( :inherit variable-pitch
                               :height 1.15))
        "")))
  "Show the name of the current buffer")

(defvar modeline-evil-state-indicator
  '(:propertize
    modeline-evil-state-cached
    display (min-width (2))
    face ( :inherit variable-pitch
           :weight bold
           :height 1.4
           :foreground headerline--evil-state-face)))

(defvar modeline-percentage-c
  '(:propertize
    "%3p"
    display (min-width (8))
    face (:weight bold :height 1.2)))

(defvar modeline-line-c
  '(:propertize
    "%4l"
    face (:weight bold :height 1.2)))

(defvar modeline-column-c
  '(:propertize
    "%c"
    display (min-width (3))
    face (:weight bold :height 1.2)))

(defvar modeline-buffer-size-c
  '(:propertize
    "%I"
    display (min-width (3))
    face (:weight bold :height 1.2)))

(defvar modeline-macro-recording-c
  `(:eval
    (when (and (modeline-active)
               (or defining-kbd-macro
                   executing-kbd-macro))
      (let ((sep (propertize ""
                             'face '( :inherit variable-pitch
                                      :height 1.1))))
        (concat sep
                (propertize (if (bound-and-true-p evil-this-macro)
                                (char-to-string evil-this-macro)
                              "Macro")
                            'face '( :inherit variable-pitch
                                     :weight bold
                                     :underline t
                                     :height 1.2))
                sep))))
  "Display current Emacs or evil macro being recorded.")

(defvar modeline-anzu-count-c
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
       'face '( :height 1.2
                :underline t))))
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for
compatibility with `evil-search'.")

(defvar modeline-flymake-c
  `(:eval
    (when (and (bound-and-true-p flymake-mode)
               (flymake-running-backends))
      (list
       flymake-mode-line-exception
       " "
       (propertize
        (headerline-flymake-count-c :error)
        ;; (concat "E" (headerline-flymake-count-c :error))
        'face '( :weight black
                 ;; :inherit variable-pitch
                 :height 1.3
                 :foreground ,headerline--err-face))
       (propertize
        (headerline-flymake-count-c :warning)
        ;; (concat "W" (headerline-flymake-count-c :warning))
        'face '( :weight black
                 ;; :inherit variable-pitch
                 :height 1.3
                 :foreground ,headerline--warn-face))
       (propertize
        (headerline-flymake-count-c :note)
        ;; (concat "N" (headerline-flymake-count-c :note))
        'face '( :weight black
                 ;; :inherit variable-pitch
                 :height 1.3
                 :foreground ,headerline--note-face)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

(defvar modeline-align-right-c
  '(:eval (propertize
           " " 'display
           `((space :align-to
                    (- (+ right right-fringe right-margin)
                       2 ;; indicator
                       5 ;; spaces
                       3 ;; magic number
                       ,(string-width
                         (format-mode-line modeline-buffer-size-c))
                       ,(when (bound-and-true-p flymake-mode)
                          (string-width
                           (format-mode-line modeline-flymake-c)))
                       ,(string-width
                         (format-mode-line mode-line-misc-info)))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.")

(defvar modeline-misc-info-c
  mode-line-misc-info
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

(defvar modeline-mlscroll-c
  '(:eval (mlscroll-mode-line))
  "Show mlscroll window scroll indicator.")

;;;;; Set `risky-local-variable'
(dolist (construct '( modeline-bg-color-change-c
                      modeline-active-indicator-with-state-c
                      modeline-active-indicator-c
                      modeline-modes-c
                      modeline-current-buffer-property-c
                      modeline-current-buffer-narrowed-c
                      modeline-current-buffer-name-c
                      modeline-evil-state-indicator
                      modeline-percentage-c
                      modeline-line-c
                      modeline-column-c
                      modeline-buffer-size-c
                      modeline-macro-recording-c
                      modeline-anzu-count-c
                      modeline-align-right-c
                      modeline-mlscroll-c
                      modeline-flymake-c
                      modeline-misc-info-c))
  (put construct 'risky-local-variable t))

(setq-default header-line-format
              '("%e "
                modeline-active-indicator-c
                modeline-bg-color-change-c
                modeline-current-buffer-property-c
                " "
                modeline-modes-c
                ;; modeline-percentage-c
                modeline-mlscroll-c
                modeline-current-buffer-narrowed-c
                modeline-line-c
                " "
                ;; modeline-column-c
                modeline-current-buffer-name-c
                " "
                modeline-macro-recording-c
                modeline-anzu-count-c
                ;; " "
                ;; modeline-evil-state-indicator
                ;; '(which-function-mode ("" which-func-format "--"))
                ;; modeline-align-right-c
                " "
                modeline-buffer-size-c
                " "
                modeline-flymake-c
                " "
                modeline-misc-info-c
                " "
                ;; modeline-active-indicator-c
                ;; " "
                ))

(setq-default mode-line-format nil)

;;; Archive
;; (setq-default header-line-format mode-line-format)
;; window-prev-sibling
;; select-window
;; selected-window
;; old-selected-window
;; (add-hook 'pre-redisplay-functions
;; (defun headerline-set-selected-window (&rest _)
;;   "what"
;;   (progn
;;     ;; (setq headerline--active-window (frame-selected-window))
;;     ;; select-window (selected-window)
;;     (if (buffer-modified-p)
;;         (update-face-remapping-alist 'headerline-modified-active 'header-line)
;;       (update-face-remapping-alist 'headerline-unmodified-active 'header-line))
;;     ;; (save-excursion
;;     (save-selected-window
;;      (select-window (old-selected-window))
;;      (if (buffer-modified-p)
;;          (update-face-remapping-alist 'headerline-modified-inactive 'header-line)
;;        (update-face-remapping-alist 'headerline-unmodified-inactive 'header-line)))));;)
;; (select-window (old-selected-window))
;; (add-to-list 'window-selection-change-functions 'headerline-set-selected-window)
;; (add-to-list 'window-state-change-functions 'headerline-set-selected-window)
;; (update-face-remapping-alist 'headerline-modified-inactive 'header-line)

;; (add-hook 'elpaca-after-init-hook 'headerline-global-mode)
;; (add-hook 'server-after-make-frame-hook 'headerline-global-mode)
;; (add-hook 'window-setup-hook 'headerline-global-mode)

;; (add-hook 'buffer-list-update-hook 'headerline-global-mode)

;; (set-face-attribute 'headerline-modified-inactive nil
;;                     :foreground fl-builtin
;;                     :background defaultbg
;;                     :box (list :line-width '(1 . 3) :color fl-builtin))
;; (set-face-attribute 'headerline-unmodified-inactive nil
;;                     :foreground fl-keyword
;;                     :background defaultbg
;;                     :box (list :line-width '(1 . 3) :color fl-keyword))

;; (if (buffer-modified-p)
;;     (update-face-remapping-alist 'headerline-modified-inactive 'header-line)
;;   (update-face-remapping-alist 'headerline-unmodified-inactive 'header-line))
;;   "Active"
;; "Inactive"))
;; '(:eval
;;   ;; (if (eq (selected-window) (frame-selected-window))
;;   (if ;; (modeline-active)
;;       (or (not (mode-line-window-selected-p))
;;       ;; (or (eq (selected-window) (frame-selected-window))
;;           (modeline-active))
;;       (if (buffer-modified-p)
;;           (update-face-remapping-alist 'headerline-modified-active 'header-line)
;;         (update-face-remapping-alist 'headerline-unmodified-active 'header-line))
;;     (if (buffer-modified-p)
;;         (update-face-remapping-alist 'headerline-modified-inactive 'header-line)
;;       (update-face-remapping-alist 'headerline-unmodified-inactive 'header-line))))

;; (defvar modeline-column-c
;;   '(:eval (propertize
;;            "%c"
;;            'display
;;            ;; `((min-width (5.0)))
;;            `((space :width
;;                     (- 7 ,(string-width
;;                            (format-mode-line "%c")))))
;;            'face '(:weight bold :height 1.25))))

;; '(:eval
;;   (when (mode-line-window-selected-p)
;;     mode-line-misc-info))

;; (if buffer-file-truename
;;     (propertize buffer-file-truename 'face '(:inherit variable-pitch :width condensed :height 1.12))))

;; (concat
;;  "buffer: " buffer-file-truename
;;  "\ngitroot: " gitroot
;;  "\ngitroot-nodash: " gitroot-nodash
;;  "\ngitroot-a: " gitroot-a
;;  "\ndirbefore: " dirbefore)
;; buffer: ~/.emacsclean/modeline.el
;; buf: /home/sys2/.emacsclean/modeline.el
;; gitroot: /home/sys2/.emacsclean/
;; gitroot-nodash: /home/sys2/.emacsclean
;; gitroot-a: .emacsclean
;; dirbefore: /home/sys2
;; (let* ((buf (buffer-file-name))
;;        (gitfolder (locate-dominating-file ".git" buffer-file-name))
;;        (gitroot (substring gitfolder 0 -5)))
;;   (concat "buf: " buf "\ngitfolder: " gitfolder "\ngitroot " gitroot "\n"
;;           (file-name-nondirectory
;;            (file-truename gitfolder))
;;           "XX"
;;           (file-truename (file-relative-name buf gitroot))))

;; (let ((file-name (buffer-file-name (buffer-base-buffer))))
;;   (unless (or (null default-directory)
;;               (null file-name)
;;               (file-remote-p file-name))
;;     ;; (when-let (project-root (project-root buffer-file-name))
;;     (when-let (project-root (locate-dominating-file ".git" file-name))
;;       (file-relative-name (or buffer-file-truename (file-truename file-name))
;;                           (concat project-root "..")))))
;; (file-relative-name (buffer-file-name) (substring (locate-dominating-file ".git" buffer-file-name) 0 -5))

;; (let ((buf (if modeline-buffer-id-cache-c modeline-buffer-id-cache-c
;;              (if buffer-file-truename buffer-file-truename
;;                ""))))
;; (propertize buf 'face
;;             '( :inherit variable-pitch
;;                :width condensed
;;                :height 1.12))

;; '(:eval (propertize
;;          "%p"
;;          'display
;;          ;; `((min-width (5.0)))
;;          `((space :width
;;                   (- 7 ,(string-width
;;                          (format-mode-line "%p")))))
;;          'face '(:weight bold :height 1.25))))

;; (defun flymake--mode-line-counters ()
;;   (when (flymake-running-backends) flymake-mode-line-counter-format))

;; (defcustom flymake-mode-line-counter-format
;;   '("["
;;     flymake-mode-line-error-counter
;;     flymake-mode-line-warning-counter
;;     flymake-mode-line-note-counter "]")

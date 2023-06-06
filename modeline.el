;;; Header Line
;; Doom's +light modeline properly modified for header-line
;;;; Variables
(defvar headerline-format-alist ())

;;;; Helper Functions
(defvar headerline--active-window (selected-window))

(defun headerline-active ()
  "Return non-nil if the selected window has an active headerline."
  (eq (selected-window) headerline--active-window))

(defun headerline-inactive ()
  "Return non-nil if the selected window has an inactive headerline."
  (not (headerline-active)))

(add-hook 'pre-redisplay-functions
          (defun headerline-set-selected-window-h (&rest _)
            "Track the active headerline's window in `headerline--active-window'."
            (let ((win (selected-window)))
              (unless (minibuffer-window-active-p win)
                (setq headerline--active-window (frame-selected-window))))))

(defun set-headerline (name &optional default)
  "Set the headerline to NAME.
If DEFAULT is non-nil, apply to all future buffers.  Headerlines
are defined with `def-headerline'."
  (if-let (format (assq name headerline-format-alist))
      (cl-destructuring-bind (lhs . (cen . rhs)) (cdr format)
        (if default
            (setq-default headerline-format-left lhs
                          headerline-format-center cen
                          headerline-format-right rhs)
          (setq headerline-format-left lhs
                headerline-format-center cen
                headerline-format-right rhs)))
    (error "Could not find %S headerline format" name)))

(defun set-headerline-hook (hooks name)
  "Set the headerline to NAME on HOOKS.
See `def-headerline' on how headerlines are defined."
  (let ((fn (intern (format "headerline-set-%s-format-h" name))))
    (dolist (hook (ensure-list hooks))
      (when after-init-time
        (dolist (name (mapcar #'car headerline-format-alist))
          (remove-hook hook (intern (format "headerline-set-%s-format-h" name)))))
      (add-hook hook fn))))

(defun def-headerline (name lhs cen rhs)
  "Define a headerline format by NAME.
LHS, CEN and RHS are the formats representing the left, center
and right hand side of the mode-line, respectively. See the
variable `format-mode-line' for details on what LHS and RHS will
accept."
  (setf (alist-get name headerline-format-alist)
        (cons lhs (cons cen rhs)))
  (fset (intern (format "headerline-set-%s-format-h" name))
        (lambda (&rest _) (set-headerline name))))

(defun update-face-remapping-alist (face target)
  (face-remap-reset-base target)
  (push (list target face) face-remapping-alist))

;;;; Macros
(defmacro def-headerline-var (name body &optional docstring &rest plist)
  "Define a headerline segment variable."
  (unless (stringp docstring)
    (push docstring plist)
    (setq docstring nil))
  `(progn
     (defconst ,name ,body ,docstring)
     ,@(if (plist-get plist :local) `((make-variable-buffer-local ',name)))
     (put ',name 'risky-local-variable t)))

(defmacro get-color-fg (face)
  "Get the :foreground property of a FACE."
  `(face-attribute ,face :foreground))

(defmacro get-color-bg (face)
  "Get the :background property of a FACE."
  `(face-attribute ,face :background))

;; (defun define-face-c (face)
;;   "Define a FACE."
;;   (defface ,face
;;     '((t :foreground "#000"
;;          :background "#000"
;;          :box (:line-width 1 :color "#000")))))

;;; Segments
(def-headerline-var headerline-format-left nil
                    "The left-hand side of the headerline."
                    :local t)
(def-headerline-var headerline-format-center nil
                    "The center of the headerline."
                    :local t)
(def-headerline-var headerline-format-right nil
                    "The right-hand side of the headerline."
                    :local t)

;;; Faces
;; (define-face-c header-line--dummy)
;; (define-face-c headerline-modified-inactive)
;; (define-face-c headerline-modified-active)
;; (define-face-c headerline-unmodified-inactive)
;; (define-face-c headerline-unmodified-active)

(defun set-headerline-faces ()
  (interactive)
  (let ((fl-keyword (get-color-fg 'font-lock-keyword-face)) ;; green
        (fl-builtin (get-color-fg 'font-lock-builtin-face)) ;; blue
        (fl-type (get-color-fg 'font-lock-type-face)) ;; brown
        (fl-variable (get-color-fg 'font-lock-variable-name-face)) ;; off-white
        (fl-string (get-color-fg 'font-lock-string-face)) ;; dark red
        (errorface (get-color-fg 'error)) ;; red
        (matchface (get-color-fg 'match)) ;; green
        (defaultfg (get-color-fg 'default)) ;; white
        (defaultbg (get-color-bg 'default))) ;; black
    ;; (message (concat fl-keyword fl-builtin fl-type))
    (set-face-attribute 'headerline-modified-inactive nil
                        :foreground fl-builtin
                        :background defaultbg
                        :box (list :line-width 1 :color fl-builtin))
    (set-face-attribute 'headerline-modified-active nil
                        :foreground fl-variable
                        :background fl-builtin
                        :box (list :line-width 1 :color fl-builtin))
    (set-face-attribute 'headerline-unmodified-inactive nil
                        :foreground fl-keyword
                        :background defaultbg
                        :box (list :line-width 1 :color fl-keyword))
    (set-face-attribute 'headerline-unmodified-active nil
                        :foreground fl-variable
                        :background fl-keyword
                        :box (list :line-width 1 :color fl-keyword))))
(set-headerline-faces) ;; init during load
(add-hook 'load-theme 'set-headerline-faces)
(add-hook 'enable-theme 'set-headerline-faces)

;; (add-hook 'pre-redisplay-functions
;; (add-hook 'redisplay
;; (defun headerline-set-selected-window (&rest _)
;;   "what"
;;   (progn
;;     (setq headerline--active-window (frame-selected-window))
;;     (if (eq (selected-window) (frame-selected-window))
;;         (if (buffer-modified-p)
;;             (update-face-remapping-alist 'headerline-modified-active 'header-line)
;;           (update-face-remapping-alist 'headerline-unmodified-active 'header-line))
;;       (if (buffer-modified-p)
;;           (update-face-remapping-alist 'headerline-modified-inactive 'header-line)
;;         (update-face-remapping-alist 'headerline-unmodified-inactive 'header-line)))))
;; (with-current-buffer (current-buffer)
;;         (if (buffer-modified-p)
;;             (update-face-remapping-alist 'headerline-modified-active 'header-line)
;;           (update-face-remapping-alist 'headerline-unmodified-active 'header-line)))

;; window-prev-sibling
;; select-window
;; selected-window
;; old-selected-window
;; (add-hook 'pre-redisplay-functions
          (defun headerline-set-selected-window (&rest _)
            "what"
            (progn
              ;; (setq headerline--active-window (frame-selected-window))
              ;; select-window (selected-window)
              (if (buffer-modified-p)
                  (update-face-remapping-alist 'headerline-modified-active 'header-line)
                (update-face-remapping-alist 'headerline-unmodified-active 'header-line))
              ;; (save-excursion
              (save-selected-window
               (select-window (old-selected-window))
               (if (buffer-modified-p)
                   (update-face-remapping-alist 'headerline-modified-inactive 'header-line)
                 (update-face-remapping-alist 'headerline-unmodified-inactive 'header-line)))));;)
              ;; (select-window (old-selected-window))
;; (add-to-list 'window-selection-change-functions 'headerline-set-selected-window)
;; (add-to-list 'window-state-change-functions 'headerline-set-selected-window)

;;;; headerline-modes
(def-headerline-var headerline-modes ; remove minor modes
                    '(" "
                      (:propertize mode-name
                                   face ,(if (headerline-active)
                                             '(region bold font-lock-keyword-face)
                                           '(region bold font-lock-builtin-face)))
                      mode-line-process
                      "%n"
                      " "))

;; (def-headerline-var headerline-modified
;;                     `(:eval
;;                       (propertize "   "
;;                                   'face (if (buffer-modified-p)
;;                                             '(:background green)
;;                                           '(:background blue)))))

;; (def-headerline-var headerline-percent
;;                     `(" "
;;                       (:propertize ,(window-start)
;;                                    face ,(if (eq (point) (point-min))
;;                                              'error
;;                                            (if (eq (point) (point-max))
;;                                                'region
;;                                              'bold)))))

;; (def-headerline-var headerline-modes
;;                   `(" "
;;                     ,mode-name
;;                     ,mode-line-process
;;                     "%n "))

(def-headerline-var headerline-line
                  `(" %l %c "))

(def-headerline-var headerline-name
                  `(" "
                    ,(buffer-file-name (buffer-base-buffer))
                    " "))

;;; Default modeline
(def-headerline :main
                `(;; "A"
                  ;; mode-line-percent
                  ;; headerline-percent
                  ;; "A"
                  headerline-modes
                  ;; "A"
                  ;; headerline-modified
                  )
                `(
                  ;; mode-line-buffer-identification
                  headerline-name
                  )
                `(
                  ;; mode-line-misc-info
                  headerline-line
                  ))

(set-headerline :main 'default)

;;; minor-mode
(define-minor-mode headerline-mode
  "Header Line Config."
  :init-value nil
  :global t
  (cond
   (headerline-mode
    (setq header-line-format
          (cons
           "" '(headerline-format-left
                (let ((sw (string-width (format-mode-line '("" headerline-format-center)))))
                  (:eval
                   (propertize
                    " "
                    'display
                    `((space :align-to (- (+
                                           center)
                                          ,(string-width
                                            (format-mode-line '("" headerline-format-right)))))))))
                headerline-format-center
                (:eval
                 (propertize
                  " "
                  'display
                  `((space :align-to (- (+ right right-fringe right-margin)
                                        ,(string-width
                                          (format-mode-line '("" headerline-format-right))))))))
                headerline-format-right))))))
(defvar mode-line-format--old (default-value 'mode-line-format))
;; (setq-default mode-line-format nil)

;; (define-global-minor-mode headerline-global-mode headerline-mode headerline-mode)

;; (add-hook 'elpaca-after-init-hook 'headerline-global-mode)
;; (add-hook 'server-after-make-frame-hook 'headerline-global-mode)
;; (add-hook 'window-setup-hook 'headerline-global-mode)

;; (add-hook 'buffer-list-update-hook 'headerline-global-mode)

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '("%l,%c"))
(setq mode-line-compact t)
;; (setq evil-mode-line-format nil)
(setq-default mode-line-format
              '("%e"
                mode-line-modified
                " modename "
                mode-name
                " remote "
                mode-line-remote
                " buffer "
                mode-line-buffer-identification
                " position "
                mode-line-position
                " VC "
                (vc-mode vc-mode)
                " misc "
                mode-line-misc-info))
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

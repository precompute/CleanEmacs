;;; Custom Faces
(custom-set-faces
 '(org-level-1 ((t (:extend t :height 1.5))))
 '(org-level-2 ((t (:extend t :height 1.3))))
 '(org-level-3 ((t (:extend t :height 1.25))))
 '(org-level-4 ((t (:extend t :height 1.15))))
 '(org-level-5 ((t (:extend t :height 1.1))))
 '(org-level-6 ((t (:extend t :height 1.08))))
 '(org-level-7 ((t (:extend t :height 1.05))))
 '(org-level-8 ((t (:extend t :height 1.05))))
 '(outline-1 ((t (:extend t :height 1.5))))
 '(outline-2 ((t (:extend t :height 1.3))))
 '(outline-3 ((t (:extend t :height 1.25))))
 '(outline-4 ((t (:extend t :height 1.15))))
 '(outline-5 ((t (:extend t :height 1.1))))
 '(outline-6 ((t (:extend t :height 1.08))))
 '(outline-7 ((t (:extend t :height 1.05))))
 '(outline-8 ((t (:extend t :height 1.05))))
 '(outline-minor-1 ((t (:extend t :height 1.5))))
 '(outline-minor-2 ((t (:extend t :height 1.3))))
 '(outline-minor-3 ((t (:extend t :height 1.25))))
 '(outline-minor-4 ((t (:extend t :height 1.15))))
 '(outline-minor-5 ((t (:extend t :height 1.1))))
 '(outline-minor-6 ((t (:extend t :height 1.08))))
 '(outline-minor-7 ((t (:extend t :height 1.05))))
 '(outline-minor-8 ((t (:extend t :height 1.05))))
 '(helpful-heading ((t (:extend t :height 1.25 :inherit variable-pitch))))
 '(transient-heading ((t (:height 1.25 :inherit variable-pitch))))
 '(org-meta-line ((t (:extend t))))
 '(org-date ((t (:inherit (list bold fixed-pitch)))))
 '(org-special-keyword ((t (:inherit (list bold fixed-pitch)))))
 '(org-ellipsis ((t (:height 1.3 :weight bold))))
 '(magit-diff-file-heading ((t (:height 1.3)))))

;;; Theme
(defvar current--default-face-foreground)
(defvar current--builtin-face-foreground)
(defvar current--keyword-face-foreground)
(defvar current--constant-face-foreground)
(defvar current--string-face-foreground)
(defvar current--default-face-background)
(defvar current--builtin-face-background)
(defvar current--keyword-face-background)
(defvar current--constant-face-background)
(defvar current--string-face-background)

(defun get-face-colors-c (&rest rest)
  (interactive)
  (setq current--default-face-foreground (face-attribute 'default :foreground))
  (setq current--builtin-face-foreground (face-attribute 'font-lock-builtin-face :foreground))
  (setq current--keyword-face-foreground (face-attribute 'font-lock-keyword-face :foreground))
  (setq current--constant-face-foreground (face-attribute 'font-lock-constant-face :foreground))
  (setq current--string-face-foreground (face-attribute 'font-lock-string-face :foreground))
  (setq current--default-face-background (face-attribute 'default :background))
  (setq current--builtin-face-background (face-attribute 'font-lock-builtin-face :background))
  (setq current--keyword-face-background (face-attribute 'font-lock-keyword-face :background))
  (setq current--constant-face-background (face-attribute 'font-lock-constant-face :background))
  (setq current--string-face-background (face-attribute 'font-lock-string-face :background)))

(defun set-org-mode-faces-c (&rest rest)
  (interactive)
  ;; (set-face-attribute 'org-todo nil
  ;;                     :underline nil
  ;;                     :inherit nil
  ;;                     :background 'unspecified)
  ;; (set-face-attribute 'org-done nil
  ;;                     :underline nil
  ;;                     :inherit nil
  ;;                     :background 'unspecified)
  (if (facep 'custom--org-mode-face-1)
      (set-face-attribute 'custom--org-mode-face-1 nil
                          :inherit 'fixed-pitch
                          :box `(:color ,current--default-face-background)
                          :background current--builtin-face-foreground))
  (if (facep 'custom--org-mode-face-2)
      (set-face-attribute 'custom--org-mode-face-2 nil
                          :inherit 'fixed-pitch
                          :box `(:color ,current--default-face-background)
                          :background current--keyword-face-foreground))
  (if (facep 'custom--org-mode-face-3)
      (set-face-attribute 'custom--org-mode-face-3 nil
                          :inherit 'fixed-pitch
                          :box `(:color ,current--default-face-background)
                          :background current--constant-face-foreground))
  (if (facep 'custom--org-mode-face-4)
      (set-face-attribute 'custom--org-mode-face-4 nil
                          :inherit 'fixed-pitch
                          :box `(:color ,current--default-face-background)
                          :background current--string-face-foreground)))

(defun set-eldoc-box-faces-c (&rest rest)
  (interactive)
  (when (facep 'eldoc-box-border)
    (set-face-attribute
     'eldoc-box-border nil
     :background (face-attribute 'shadow :foreground))
    (set-face-attribute
     'eldoc-box-body nil
     :background (face-attribute 'region :background))))

(dolist (f '(set-eldoc-box-faces-c
             set-org-mode-faces-c
             get-face-colors-c))
  (add-to-list 'enable-theme-functions f))

;;;; Load Theme
(load-theme 'sculpture-themes-dark t)

;;;; Reload Theme
(defun reload-theme-c (&rest rest)
  "Reload the first theme in `custom-enabled-themes’.
Ignore REST."
  (interactive)
  (let ((theme (car custom-enabled-themes)))
    (disable-theme theme)
    (enable-theme theme))
  (set-fonts-c))
(add-to-list 'after-make-frame-functions 'reload-theme-c)

;;; display-buffer-alist
(setq window-sides-slots '(1 1 1 1)) ;; LTRB; This is a good enough default.
(setq display-buffer-alist
      '(("\\*info\\*" display-buffer-in-direction
         (direction . bottom)
         (window-height . 0.3)
         (body-function . select-window)
         (window-parameters (no-delete-other-windows . t)))
        ("\\*Help\\*" display-buffer-in-direction
         (direction . bottom)
         (window-height . 0.3)
         (body-function . select-window)
         (window-parameters (no-delete-other-windows . t)))
        ("\\*helpful[^z-a]*" display-buffer-in-direction ;; (rx "*helpful" (* anything))
         (direction . bottom)
         (window-height . 0.35)
         (body-function . select-window)
         (window-parameters (no-delete-other-windows . t)))
        ("\\*Messages\\*" display-buffer-in-direction
         (direction . bottom)
         (window-height . 0.40)
         (body-function . select-window)
         (window-parameters (no-delete-other-windows . t)))
        ("\\*transient\\*")
        ("[[:space:]][^z-a]*transient\\*") ;; transient--buffer-name
        ("[[:space:]]\\*undo-tree\\*")
        ("[[:space:]]\\*vundo tree\\*")
        ((mode . vterm-mode) display-buffer-same-window
         (body-function . select-window))
        ("\\*[^z-a]*" display-buffer-in-direction ;; (rx "*" (* anything))
         (direction . bottom)
         (window-height . 0.40)
         (body-function . select-window)
         (window-parameters (no-delete-other-windows . t)))))

;;; Revert on focus-in
(when (fboundp 'revert-buffer-if-not-modified)
  (add-function
   :after after-focus-change-function
   'revert-buffer-if-not-modified))

;;; other
;;;; show-paren
(setq show-paren-delay 0.2
      show-paren-style 'expression)

;;;; transparency
(add-hook 'window-setup-hook 'set-transparency-c)
(add-to-list 'initial-frame-alist (cons 'alpha-background transparency-value-c))

;;; Fonts
(defun set-fonts-c (&rest rest)
  "Set fonts for Emacs."
  (interactive)
  (progn
    (set-face-font 'default (font-spec :family "JuliaMono" :size 12 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "GT Alpina Typewriter" :size 17 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "TX-02" :size 14 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Pragmata Pro" :size 15 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Letter Gothic Mono Pro" :size 16 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Inconsolata" :size 15 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Gohu GohuFont" :size 12 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Cozette" :size 14 :weight 'regular))

    ;; (set-face-font 'variable-pitch (font-spec :family "Meta Corr Pro" :size 15))
    ;; (set-face-font 'variable-pitch (font-spec :family "Sabon LT Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Cisalpin LT Std" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Adobe Caslon Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Palatino eText" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Minion Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "SangBleu Kingdom" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Pressura" :size 17 :width 'condensed :weight 'regular))
    (set-face-font 'variable-pitch (font-spec :family "GT Alpina" :size 17 :width 'condensed :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Flexa" :size 19 :width 'expanded :weight 'light))
    ;; (set-face-font 'variable-pitch (font-spec :family "Franziska Pro" :size 18))
    ;; (set-face-font 'variable-pitch (font-spec :family "Angie Pro" :size 15 :slant nil))
    ;; (set-face-font 'variable-pitch (font-spec :family "Dagny Offc Pro" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "Inter" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica Neue eText Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica Neue LT W1G" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Univers LT Std" :width 'expanded :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Info Text Offc Pro" :size 15))
    ;; (set-face-font 'variable-pitch (font-spec :family "DIN Round Offc Pro" :size 17))
    ;; (set-face-font 'variable-pitch (font-spec :family "Alegreya Sans" :size 18))

    ;; (set-face-font 'default ":antialias=false:hinting=true") ;; feeling edgy?
    (copy-face 'default 'fixed-pitch)))
(set-fonts-c)
;; (set-fontset-font t 'unicode (font-spec :family "Symbola"))

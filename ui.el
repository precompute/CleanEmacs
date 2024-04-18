;;; Theme
(defun set-theme-c ()
  "Set theme for Emacs."
  (interactive)
  (progn
    (require-theme 'sculpture-themes)
    (load-theme 'sculpture-themes-dark t)))
(set-theme-c)

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
 '(org-ellipsis ((t (:height 1.29 :weight bold))))
 '(magit-diff-file-heading ((t (:height 1.2)))))

(defun get-face-colors-c (&rest rest)
  (interactive)
  (defvar current--default-face-foreground (face-attribute 'default :foreground))
  (defvar current--builtin-face-foreground (face-attribute 'font-lock-builtin-face :foreground))
  (defvar current--keyword-face-foreground (face-attribute 'font-lock-keyword-face :foreground))
  (defvar current--constant-face-foreground (face-attribute 'font-lock-constant-face :foreground))
  (defvar current--string-face-foreground (face-attribute 'font-lock-string-face :foreground))
  (defvar current--default-face-background (face-attribute 'default :background))
  (defvar current--builtin-face-background (face-attribute 'font-lock-builtin-face :background))
  (defvar current--keyword-face-background (face-attribute 'font-lock-keyword-face :background))
  (defvar current--constant-face-background (face-attribute 'font-lock-constant-face :background))
  (defvar current--string-face-background (face-attribute 'font-lock-string-face :background)))
(get-face-colors-c)
(add-to-list 'enable-theme-functions 'get-face-colors-c)

(defun set-org-mode-faces-c (&rest rest)
  (interactive)
  (set-face-attribute 'org-todo nil
                      :underline nil
                      :inherit nil
                      :background 'unspecified)
  (set-face-attribute 'org-done nil
                      :underline nil
                      :inherit nil
                      :background 'unspecified)
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
(set-org-mode-faces-c)
(add-to-list 'enable-theme-functions 'set-org-mode-faces-c)

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
(if (fboundp 'revert-buffer-if-not-modified)
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
    ;; (set-face-font 'default (font-spec :family "NeueJuliaMono" :size 15 :weight 'regular)) ;; reduces X-height somehow
    (set-face-font 'default (font-spec :family "JuliaMono" :size 12 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Inconsolata" :size 15 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Gohu GohuFont" :size 12 :weight 'regular))
    ;; (set-face-font 'default (font-spec :family "Cozette" :size 14 :weight 'regular))

    (set-face-font 'variable-pitch (font-spec :family "Meta Corr Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Sabon LT Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Cisalpin LT Std" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Adobe Caslon Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Palatino eText" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Minion Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Dagny Offc Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Inter" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica Neue eText Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica Neue LT W1G" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Univers LT Std" :width 'expanded :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Info Text Offc Pro" :size 15))
    ;; (set-face-font 'variable-pitch (font-spec :family "DIN Round Offc Pro" :size 17))

    ;; (set-face-font 'default ":antialias=false:hinting=true") ;; feeling edgy?
    (copy-face 'default 'fixed-pitch)))
(set-fonts-c)
;; (set-fontset-font t 'unicode (font-spec :family "Symbola"))

;;; Custom Faces
(cl-loop for level from 1 to 8 for size in '(1.6 1.525 1.45 1.375 1.3 1.225 1.15 1.075)
         do (dolist (z '("org-level-%s" "outline-%s" "outline-minor-%s"))
              (custom-set-faces `(,(intern (format z level)) ((t :extend t :height ,size))))))
(custom-set-faces
 '(helpful-heading ((t :extend t :height 1.25 :inherit variable-pitch)))
 '(transient-heading ((t :height 1.25 :inherit variable-pitch)))
 '(org-meta-line ((t :extend t)))
 '(org-date ((t :inherit (bold fixed-pitch))))
 '(org-special-keyword ((t :inherit (bold fixed-pitch))))
 '(org-ellipsis ((t :height 1.3 :weight bold)))
 '(magit-diff-file-heading ((t :height 1.3))))

;;; Theme
(defun get-face-colors-c (&rest rest)
  "Generate custom face color variables from the active theme’s colors.  Ignore REST."
  (interactive)
  (dolist (z (cl-loop for z in '( default region error success
                                  builtin keyword constant string regexp type doc)
                      append (cl-loop for y in '(background foreground)
                                      collect (list (if (memq z '(default region error success)) z
                                                      (intern (format "font-lock-%s-face" z)))
                                                    (intern (format "current--%s-face-%s" z y))
                                                    (intern (format ":%s" y))))))
    (let* ((face (car z))
           (var (nth 1 z))
           (type (nth 2 z))
           (val (when (facep face) (face-attribute face type))))
      (if (eq val 'unspecified) (set var nil) (set var val)))))

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
  (when (and (featurep 'eldoc-box) (facep 'eldoc-box-border))
    (set-face-attribute
     'eldoc-box-border nil
     :background (face-attribute 'shadow :foreground))
    (set-face-attribute
     'eldoc-box-body nil
     :background (face-attribute 'region :background))))

(defun set-pulsar-face-c (&rest _args)
  (interactive)
  (when (featurep 'pulsar)
    (set-face-attribute 'pulsar-generic nil
                        :background (apply #'color-rgb-to-hex
                                           (color-blend
                                            (color-blend (color-name-to-rgb current--region-face-background)
                                                         (color-name-to-rgb current--default-face-foreground)
                                                         0.85)
                                            (color-name-to-rgb current--success-face-foreground)
                                            0.85))
                        :inherit nil)))

(defun set-dired-posframe-face-c (&rest _args)
  (interactive)
  (when (featurep 'dired-posframe)
    (set-face-attribute 'dired-posframe-border nil :background current--builtin-face-foreground :inherit nil)))

(dolist (f '(set-pulsar-face-c
             set-dired-posframe-face-c
             set-eldoc-box-faces-c
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
(setq transparency-value-c 97)

;;;; window-divider
(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode)

;;; Fonts
(defface fixed-pitch-numbers '((t)) "Face for fixed-pitch numbers.")

(defun set-face-font-c (spec)
  "Set SPEC to faces `default’ and `fixed-pitch’.
`copy-face’ brings over the background setting, and more."
  (set-face-font 'default spec)
  (set-face-font 'fixed-pitch spec))

(defun set-fonts-c (&rest rest)
  "Set fonts for Emacs."
  (interactive)
  (progn
    ;; (set-face-font-c (font-spec :family "JuliaMono" :size 13 :weight 'medium))
    ;; (set-face-font-c (font-spec :family "Luculent" :size 12))
    (set-face-font-c (font-spec :family "GT Standard Mono" :size 15 :weight 'medium :width 'condensed))
    ;; (set-face-font-c (font-spec :family "GT Alpina Typewriter" :size 16 :weight 'light))
    ;; (set-face-font-c (font-spec :family "GT Alpina Typewriter" :size 16 :weight 'regular))
    ;; (set-face-font-c (font-spec :family "GT Flexa Mono" :size 14 :weight 'light))
    ;; (set-face-font-c (font-spec :family "GT America Mono LCG" :size 14))
    ;; (set-face-font-c (font-spec :family "TX-02" :size 12 :weight 'regular))
    ;; (set-face-font-c (font-spec :family "Iosevka" :size 14))
    ;; (set-face-font-c (font-spec :family "Pragmata Pro" :size 15 :weight 'regular))
    ;; (set-face-font-c (font-spec :family "Letter Gothic Mono Pro" :size 16 :weight 'regular))
    ;; (set-face-font-c (font-spec :family "Inconsolata" :size 15 :weight 'regular))
    ;; (set-face-font-c (font-spec :family "Inconsolata" :size 15 :weight 'regular :width 'condensed))
    ;; (set-face-font-c (font-spec :family "Gohu GohuFont" :size 12 :weight 'regular))
    ;; (set-face-font-c (font-spec :family "Cozette" :size 14 :weight 'regular))

    ;; (set-face-font 'variable-pitch (font-spec :family "Meta Corr Pro" :size 15))
    ;; (set-face-font 'variable-pitch (font-spec :family "Sabon LT Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Cisalpin LT Std" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Adobe Caslon Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Palatino eText" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Frutiger Serif LT Pro" :size 14 :width 'condensed))
    ;; (set-face-font 'variable-pitch (font-spec :family "Myriad Pro" :size 15 :width 'semi-condensed))
    ;; (set-face-font 'variable-pitch (font-spec :family "SuisseIntl" :size 15 :width 'condensed))
    ;; (set-face-font 'variable-pitch (font-spec :family "Minion Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "SangBleu Kingdom" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Pressura" :size 17 :width 'condensed :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Alpina" :size 15 :width 'regular :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Alpina" :size 16 :width 'regular :weight 'light))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Alpina" :size 16 :width 'regular :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Alpina" :size 16 :width 'condensed :weight 'light))
    (set-face-font 'variable-pitch (font-spec :family "GT Alpina" :size 16 :width 'condensed :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Flexa" :size 16 :width 'expanded :weight 'light))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Flexa" :size 16 :width 'condensed :weight 'light))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT America LCG" :size 14 :width 'narrow :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT America LCG" :size 16 :width 'expanded :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Super Text" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Sectra" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Standard" :size 16 :weight 'normal :width 'condensed))
    ;; (set-face-font 'variable-pitch (font-spec :family "GT Maru" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "Franziska Pro" :size 13))
    ;; (set-face-font 'variable-pitch (font-spec :family "Source Sans 3" :size 15))
    ;; (set-face-font 'variable-pitch (font-spec :family "Source Serif 4" :size 15))
    ;; (set-face-font 'variable-pitch (font-spec :family "Angie Pro" :size 15 :slant nil))
    ;; (set-face-font 'variable-pitch (font-spec :family "Dagny Offc Pro" :size 15))
    ;; (set-face-font 'variable-pitch (font-spec :family "Inter" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Larsseit" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica Neue eText Pro" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Helvetica Neue LT W1G" :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Zwo Offc Pro" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "Megano Offc Pro" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "Univers LT Std" :width 'expanded :size 14))
    ;; (set-face-font 'variable-pitch (font-spec :family "Info Text Offc Pro" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "DIN Round Offc Pro" :size 16))
    ;; (set-face-font 'variable-pitch (font-spec :family "Alegreya Sans" :size 18))
    ;; (set-face-font 'variable-pitch (font-spec :family "Caecilia eText" :size 12))

    (set-face-font 'fixed-pitch-numbers (font-spec :family "Info Text Offc Pro" :size 16))
    ;; (set-face-font 'fixed-pitch-numbers (font-spec :family "Meta Serif SC Offc Pro" :size 17 :weight 'bold))

    ;; (set-face-font 'default ":antialias=false:hinting=true") ;; feeling edgy?
    ;; (copy-face 'default 'fixed-pitch)
    (set-fontset-font t 'symbol (font-spec :family "Symbola"))
    (set-face-attribute 'bold nil :weight 'extra-bold)))
(set-fonts-c)

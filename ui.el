;;; Theme
(require-theme 'sculpture-themes)
(load-theme 'sculpture-themes-dark t)

;;; Custom Faces
(custom-set-faces
 '(org-level-1 ((t (:extend t :height 1.5))))
 '(org-level-2 ((t (:extend t :height 1.3))))
 '(org-level-3 ((t (:extend t :height 1.2))))
 '(org-level-4 ((t (:extend t :height 1.1))))
 '(org-level-5 ((t (:extend t :height 1.07))))
 '(org-level-6 ((t (:extend t :height 1.03))))
 '(org-level-7 ((t (:extend t))))
 '(org-level-8 ((t (:extend t))))
 '(helpful-heading ((t (:extend t))))
 '(org-meta-line ((t (:extend t)))))

;;; other
;;;; show-paren
(setq show-paren-delay 0.2
      show-paren-style 'expression)

;;; Fonts
(set-face-font 'default (font-spec :family "JuliaMono" :size 14 :weight 'regular))
(set-face-font 'variable-pitch (font-spec :family "Meta Corr Pro" :size 17))
(copy-face 'default 'fixed-pitch)


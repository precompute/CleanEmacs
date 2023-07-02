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
 '(helpful-heading ((t (:extend t :height 1.25 :inherit 'variable-pitch))))
 '(transient-heading ((t (:height 1.25 :inherit 'variable-pitch))))
 '(org-meta-line ((t (:extend t))))
 '(org-date ((t (:inherit (list bold fixed-pitch))))))

;;; display-buffer-alist
(setq switch-to-buffer-obey-display-actions t) ;; makes cursor jump to new window

(setq window-sides-slots '(0 0 0 1)) ;; LTRB; This is a good enough default.
(setq display-buffer-alist nil)
(dolist (prop '(("\\*info\\*" display-buffer-in-direction
                 (direction . bottom)
                 (window-height . 0.3)
                 (window-parameters (no-delete-other-windows . t)))
                ("\\*Help\\*" display-buffer-in-direction
                 (direction . bottom)
                 (window-height . 0.3)
                 (window-parameters (no-delete-other-windows . t)))
                ("\\*helpful[^z-a]*" display-buffer-in-direction ;; (rx "*helpful" (* anything))
                 (direction . bottom)
                 (window-height . 0.35)
                 (window-parameters (no-delete-other-windows . t)))
                ("\\*Messages\\*" display-buffer-in-direction
                 (direction . bottom)
                 (window-height . 0.40)
                 (window-parameters (no-delete-other-windows . t)))
                ("\\*[^z-a]*" display-buffer-in-direction ;; (rx "*" (* anything))
                 (direction . bottom)
                 (window-height . 0.40)
                 (window-parameters (no-delete-other-windows . t)))))
  (add-to-list 'display-buffer-alist prop))

;;; other
;;;; show-paren
(setq show-paren-delay 0.2
      show-paren-style 'expression)

;;; Fonts
(defun set-fonts-c (&rest rest)
  "Set fonts for Emacs."
  (interactive)
  (progn
    (set-face-font 'default (font-spec :family "JuliaMono" :size 14 :weight 'regular))
    ;; (set-face-font 'variable-pitch (font-spec :family "Meta Corr Pro" :size 17))
    ;; (set-face-font 'variable-pitch (font-spec :family "Dagny Offc Pro" :size 17))
    (set-face-font 'variable-pitch (font-spec :family "DIN Round Offc Pro" :size 17))
    (copy-face 'default 'fixed-pitch)))
(set-fonts-c)

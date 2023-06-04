;;; Theme
(require-theme 'sculpture-themes)
(load-theme 'sculpture-themes-dark t)

;; ;;; eldoc
;; (defadvice :around #'elisp-get-var-docstring
;;             "Display variable value next to documentation in eldoc."
;;             emacs-lisp-append-value-to-eldoc-a (fn sym)
;;             (when-let (ret (funcall fn sym))
;;               (if (boundp sym)
;;                   (concat ret " "
;;                           (let* ((truncated " [...]")
;;                                  (print-escape-newlines t)
;;                                  (str (symbol-value sym))
;;                                  (str (prin1-to-string str))
;;                                  (limit (- (frame-width) (length ret) (length truncated) 1)))
;;                             (format (format "%%0.%ds%%s" (max limit 0))
;;                                     (propertize str 'face 'warning)
;;                                     (if (< (length str) limit) "" truncated))))
;;                 ret)))


;;; Custom Faces
;; (require 'org-mode)
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

;;; Header Line
;; (setq mode-line-percent-position '(-3 "%p"))
;; (setq mode-line-position-column-line-format '("%l,%c"))
;; (setq mode-line-compact t)
;; ;; (setq evil-mode-line-format nil)
;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-modified
;;                 " A "
;;                 mode-name
;;                 " A "
;;                 mode-line-remote
;;                 " A "
;;                 mode-line-buffer-identification
;;                 " A "
;;                 mode-line-position
;;                 " A "
;;                 (vc-mode vc-mode)
;;                 " A "
;;                 mode-line-misc-info))
;; (setq header-line-format mode-line-format)
;; (setq mode-line-format nil)

;;; Fonts
(set-face-font 'default (font-spec :family "JuliaMono" :size 14 :weight 'regular))
(set-face-font 'variable-pitch (font-spec :family "Meta Corr Pro" :size 17))
(copy-face 'default 'fixed-pitch)


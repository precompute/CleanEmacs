;; -*- lexical-binding: t; -*-
(use-package vertico
  :ensure ( :host github
            :repo "minad/vertico"
            :files ("*.el" "extensions/*.el"))
  :bind
  (:map vertico-map
        ("DEL" . vertico-directory-delete-char) ;; delete entire folder names
        ("C-SPC" . vertico-quick-insert)
        ("C-z" . vertico-quick-exit)
        ("C-<up>" . vertico-previous-group)
        ("C-<down>" . vertico-next-group))
  :custom
  (vertico-count 25)
  (vertico-scroll-margin 5)
  (vertico-cycle t)
  ;; (vertico-count-format ;; slows things down
  ;;  (cons (propertize "%-10s" 'face 'font-lock-keyword-face)
  ;;        (concat
  ;;         (propertize "  %s" 'face 'font-lock-builtin-face)
  ;;         " %s")))
  (vertico-quick1 "mneio;")
  (vertico-quick2 "jlukh,")
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy) ;; entering ~/ or //
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save) ;; save vertico for reinvocation
  ;; (vertico-reverse-mode)

  ;; Adapted from https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (defface vertico-prefix-face-c
    '((t :foreground "green1"))
    "Face for Vertico's custom current arrow.")
  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not (bound-and-true-p vertico-flat-mode)) (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (concat (propertize (if (= vertico--index index)
                            "◼ " "◻ ")
                            ;; "▰ " "▱ ")
                            ;; "◆ " "◇ ")
                        'face 'vertico-prefix-face-c) cand))

  ;; Add number of groups to count string
  (defun vertico-format-count-c ()
    "Add the length of `vertico--groups' to vertico--format-count.
Ignore `vertico-count-format'."
    (format "%-8s"
            (format " [%d] %s/%s "
                    (length vertico--groups)
                    (cond ((>= vertico--index 0) (1+ vertico--index))
                          (vertico--allow-prompt "*")
                          (t "!"))
                    vertico--total)))
  (advice-add #'vertico--format-count :override #'vertico-format-count-c)

  :init
  (vertico-mode))

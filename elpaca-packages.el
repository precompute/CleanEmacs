;; elpaca-packages.el -*- lexical-binding: t; -*-
(defun elpacaLF (package &optional inbuilt?)
  (let ((path (expand-file-name (concat "packages/"
                                        (if inbuilt? "inbuilt/")
                                        package ".el")
                                user-emacs-directory)))
    (if (not (file-exists-p path))
        (progn
          (message
           (concat "Creating config file for package `"
                   package "'."))
          (save-excursion
            (write-region (concat "(use-package "
                                  package
                                  (if inbuilt? "\n:elpaca nil")
                                  ")")
                          nil
                          path))))
    (load-file path)))

;;; Evil
(elpacaLF "evil")
(elpacaLF "evil-anzu") ;; number of matches
(elpacaLF "evil-args") ;; textobjects
(elpacaLF "evil-collection") ;; keybinds
(elpacaLF "evil-exchange") ;; exchange
(elpacaLF "evil-lion") ;; align
(elpacaLF "evil-nerd-commenter") ;; comments
(elpacaLF "evil-numbers") ;; numbers
(elpacaLF "evil-goggles") ;; visual hints
(elpacaLF "evil-snipe") ;; quick jump
(elpacaLF "evil-surround") ;; delimiter add/remove/replace
(elpacaLF "evil-visualstar") ;; search with *
;;; Themes
;;;; Sculpture-Themes 
(elpaca (sculpture-themes :repo "/home/sys2/temp/git/t-e-r-m/sculpture-themes/"))
;;; Keybinds
;;;; General
(elpacaLF "general")
;;; Completion
;;;; Vertico
(elpacaLF "vertico")
;;;;; Orderless (sorting)
(elpacaLF "orderless")
;;;;; Marginalia (vertico hints)
(elpacaLF "marginalia")
;;;;; Corfu (hints)
(elpacaLF "corfu")
;;;;; Consult (incremental functions)
(elpacaLF "consult")
;;; Magit
(elpacaLF "magit")
;;;; Magit Section
(elpacaLF "magit-section")
;;; Undo
;; (elpacaLF "undo-tree")
(elpacaLF "undo-fu")
;;;; Undo persistence
(elpacaLF "undo-fu-session")
;;; Olivetti
(elpacaLF "olivetti")
;;; Indent Guides
(elpacaLF "highlight-indent-guides")
;;; Which-Key
(elpacaLF "which-key")
;;; Rainbow Delimiters
(elpacaLF "rainbow-delimiters")
;;; Helpful (help buffers)
(elpacaLF "helpful")
;;; Moody (modeline)
;; (elpacaLF "moody") ;; doesn't work on the header-line
;;;; Minions (hide modes on modeline)
;; (elpacaLF "minions")
;;; Ligature
(elpacaLF "ligature")
;;; Mixed-Pitch
(elpacaLF "mixed-pitch")
;;; Faces for `outline-mode'
(elpacaLF "outline-minor-faces")
;;; Highlight Numbers
(elpacaLF "highlight-numbers")
;;; Highlight Quoted
(elpacaLF "highlight-quoted")
;;; imenu
(elpacaLF "imenu-list")
;; (elpacaLF "flimenu") ;; flatten imenu
;; (elpacaLF "keycast") ;; show keys in modeline
;; (elpacaLF "bicycle") ;; cyclic imenu entries
;;; perspective (workspace)
;; (elpacaLF "perspective")
;;; diff-hl (git info in fringe)
(elpacaLF "diff-hl")
;;; smart modeline
;; (elpacaLF "smart-mode-line")
;;; mood-line
(elpacaLF "mood-line")



;;; Dired
(elpacaLF "dired" t)
;;;; Dired Faces (font lock)
(elpacaLF "diredfl")
;;; Savehist (for minibuffers)
(elpacaLF "savehist" t)
;;; Transient
(elpacaLF "transient" t)
;;; Outline
(elpacaLF "outline" t)

;;;; Org
(elpacaLF "org" t)
;;;;; Org Bullets
(elpacaLF "org-bullets")
;;;;; Org Super Agenda
(elpacaLF "org-super-agenda")

;;; Language
;;;; Elisp
(elpacaLF "elisp-slime-nav")
;;;; Markdown
(elpacaLF "markdown-mode")


;;; Misc
;;;; sqlite3
(elpacaLF "sqlite3")

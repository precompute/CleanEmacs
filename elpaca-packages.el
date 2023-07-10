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
                                  (if inbuilt? "\n:elpaca nil)"
                                    "\n:elpaca (:depth 1)\n)"))
                          nil
                          path))))
    (load-file path)))

;;; Evil
(elpacaLF "evil")
;;;; number of matches
(elpacaLF "evil-anzu")
;;;; textobjects
(elpacaLF "evil-args")
;;;; keybinds
(elpacaLF "evil-collection")
;;;; exchange
(elpacaLF "evil-exchange")
;;;; align
(elpacaLF "evil-lion")
;;;; comments
(elpacaLF "evil-nerd-commenter")
;;;; numbers
(elpacaLF "evil-numbers")
;;;; visual hints
(elpacaLF "evil-goggles")
;;;; quick jump
(elpacaLF "evil-snipe")
;;;; delimiter add/remove/replace
(elpacaLF "evil-surround")
;;;; search with *
(elpacaLF "evil-visualstar")
;;; Themes
;;;; Sculpture-Themes
;; (elpaca (sculpture-themes-local :repo "/home/sys2/temp/git/t-e-r-m/sculpture-themes/"))
(elpacaLF "sculpture-themes")
;;; Keybinds
;;;; General
(elpacaLF "general")
;;; Completion
;;;; Vertico
(load-file (expand-file-name "packages/vertico.el" user-emacs-directory))
;;;;; Orderless (sorting)
(elpacaLF "orderless")
;;;;; Marginalia (vertico hints)
(elpacaLF "marginalia")
;;;;; Corfu (hints)
(load-file (expand-file-name "packages/corfu.el" user-emacs-directory))
;;;;;; Candidate overlay
(elpacaLF "corfu-candidate-overlay")
;;;;;; Icons
(elpacaLF "kind-icon")
;;;;; Embark (Add-on functions)
(elpacaLF "embark")
;;;;;; embark-consult
(elpacaLF "embark-consult")
;;;;;; wgrep (for embark-occur)
(elpacaLF "wgrep")
;;;;; Consult (incremental functions)
(elpacaLF "consult")
;;; Magit
(elpacaLF "magit")
;;;; Magit Section
(elpacaLF "magit-section")
;;; Undo
;; (elpacaLF "undo-tree")
;; (elpacaLF "undo-fu")
(elpacaLF "vundo")
;;;; Undo persistence
;; (elpacaLF "undo-fu-session")
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
(elpacaLF "flimenu") ;; flatten imenu
(elpacaLF "keycast") ;; show keys in modeline
(elpacaLF "bicycle") ;; cyclic imenu entries
;;; perspective (workspace)
(elpacaLF "perspective")
;;; diff-hl (git info in fringe)
(elpacaLF "diff-hl")
;;; transpose-frame
(elpacaLF "transpose-frame")
;;; rainbow-mode (highlight colors)
(elpacaLF "rainbow-mode")
;;; whitespace-cleanup-mode
(elpacaLF "whitespace-cleanup-mode")
;;; outshine-mode (custom headings in arbitrary code files)
(elpacaLF "outshine")
;;; PDF Tools
(elpacaLF "pdf-tools")
;;; Alarm Clock
(elpacaLF "alarm-clock")



;;; Dired
(elpacaLF "dired" t)
;;;; Dired Faces (font lock)
(elpacaLF "diredfl")
;;;; Dired Sidebar
(elpacaLF "dired-sidebar")
;;;;; Dired Subtree
(elpacaLF "dired-hacks-utils")
;;;; Dired Narrow
(elpacaLF "dired-narrow")
;;;; Dired Collapse (skip empty directories)
(elpacaLF "dired-collapse")
;;; Savehist (for minibuffers)
(elpacaLF "savehist" t)
;;; Transient
(elpacaLF "transient" t)
;;; Outline
(elpacaLF "outline" t)
;;; Flymake (syntax errors)
(elpacaLF "flymake" t)
;;; Tree-Sitter (syntax highlighting)
;; (elpacaLF "treesit" t)
;;; Eldoc (documentation in echo area)
(elpacaLF "eldoc" t)
;;;; Eldoc Box (documentation in childframe)
(elpacaLF "eldoc-box")
;;; Electric Indent
(elpacaLF "electric" t)
;;; imenu
(elpacaLF "imenu" t)
;;; project
(elpacaLF "project" t)
;;; Org
(elpacaLF "org" t)
;;;; Org Agenda
(elpacaLF "org-agenda" t)
;;;; Org Bullets
(elpacaLF "org-bullets")
;;;; Org Super Agenda
(elpacaLF "org-super-agenda")
;;;; Show Markup when under point
(elpacaLF "org-appear")

;;; Language
;;;; Elisp
(elpacaLF "elisp-slime-nav")
;;;; Markdown
(elpacaLF "markdown-mode")
;;;; Lua
(elpacaLF "lua-mode")
;;;; Go
(elpacaLF "go-mode")


;;; Misc
;;;; sqlite3
(elpacaLF "sqlite3")
;;;; xr (inverse of rx)
(elpacaLF "xr")
;;;; relint (regexp linter)
(elpacaLF "relint")
;;;; occurx (filter lines in buffer)
(elpacaLF "occurx-mode")
;;;; Terminal (if I ever use it)
(elpacaLF "vterm")
;;;; Fontification and colors for info-mode
(elpacaLF "info-colors")
;;;; Browse older revisions of a file in a git repo
(elpacaLF "git-timemachine")

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
                                  (if inbuilt? "\n:ensure nil)"
                                    "\n:ensure (:depth 1)\n)"))
                          nil
                          path))))
    (load-file path)))

(defmacro elpacaC (package)
  "Load a custom package."
  `(load-file (expand-file-name
               (concat ,package ".el")
               (concat user-emacs-directory "packages/"))))

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
;;;; indentation textobjects
(elpacaLF "evil-indent-plus")
;;; Themes
;;;; Sculpture-Themes
(elpacaLF "sculpture-themes")
;;;; Hyperstitional-Themes
(elpacaLF "hyperstitional-themes")
;;; Keybinds
;;;; General
(elpacaLF "general")
;;; Completion
;;;; Vertico
(elpacaC "vertico")
;;;;; Orderless (sorting)
(elpacaLF "orderless")
;;;;; Marginalia (vertico hints)
(elpacaLF "marginalia")
;;;;; Corfu (hints)
(elpacaC "corfu")
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
;;;;; Vertico-Posframe (center child frame)
(elpacaLF "vertico-posframe")
;;; Magit
(elpacaLF "magit")
;;;; Magit Section
(elpacaLF "magit-section")
;;; Undo
;; (elpacaLF "undo-tree")
;; (elpacaLF "undo-fu")
(elpacaLF "vundo")
;;;; Undo persistence
(elpacaLF "undo-fu-session")
;; ;;; Olivetti
;; (elpacaLF "olivetti") ;; Slows everything down!
;; ;;; Perfect Margin
;; (elpacaLF "perfect-margin") ;; Better than olivetti!
;;; Indent Guides
(elpacaLF "highlight-indent-guides")
;; (elpacaC "indent-bars")
;;; Which-Key
(elpacaLF "which-key")
;;; Rainbow Delimiters
(elpacaLF "rainbow-delimiters")
;;; Helpful (help buffers)
(elpacaLF "helpful")
;; ;;; Ligature
;; (elpacaLF "ligature")
;;; Mixed-Pitch
(elpacaC "mixed-pitch")
;;; Faces for `outline-mode'
(elpacaLF "outline-minor-faces")
;;; Highlight Numbers
(elpacaLF "highlight-numbers")
;;; Highlight Quoted
(elpacaLF "highlight-quoted")
;;; Highlight Defined
(elpacaLF "highlight-defined") ;; Slows everything down
;;; imenu
(elpacaLF "imenu-list")
(elpacaLF "flimenu") ;; flatten imenu
(elpacaLF "keycast") ;; show keys in modeline
(elpacaLF "bicycle") ;; cyclic imenu entries
;;; perspective (workspace)
(elpacaLF "perspective")
;; ;;;; frame integration
;; (elpacaLF "nameframe")
;; (elpacaLF "nameframe-project")
;; (elpacaLF "nameframe-perspective")
;;; diff-hl (git info in fringe)
(elpacaLF "diff-hl")
;;; transpose-frame
(elpacaLF "transpose-frame")
;;; rainbow-mode (highlight colors)
(elpacaLF "rainbow-mode")
;;; whitespace-cleanup-mode
(elpacaLF "whitespace-cleanup-mode")
;; ;;; outshine-mode (custom headings in arbitrary code files)
;; (elpacaLF "outshine")
;; ;;;; Backline
;; (elpacaLF "backline")
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
;;;; Dired Subtree
(elpacaLF "dired-subtree")
;;;; Dired Posframe
(elpacaLF "dired-posframe")
;; ;;;; Dired Hist
;; (elpacaLF "dired-hist")
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
;;;; Sliced Images
(elpacaLF "org-sliced-images")
;; ;;; Hyperbole
;; (elpacaLF "hyperbole")
;;; Hippie-Expand
(elpacaLF "hippie-exp" t)
;;; Ispell
(elpacaLF "ispell" t)
;;; Email
(elpacaLF "notmuch")
;;; Language
;;;; Prog-mode
(elpacaLF "prog-mode" t)
;;;; Eglot
(elpacaLF "eglot" t)
;;;; Elisp
(elpacaLF "elisp-slime-nav")
;;;; Markdown
(elpacaLF "markdown-mode")
;;;; Lua
(elpacaLF "lua-mode")
;;;; Go
(elpacaLF "go-mode")
;;;; Julia
(elpacaLF "julia-mode")
;;;;; eglot support
(elpacaLF "eglot-jl")
;;;; CSV
(elpacaLF "csv-mode")
;;;; Lisp-Flavored Erlang
(elpacaLF "lfe-mode")
;;;; Javascript
(elpacaLF "js" t)
;;;; Rust
(elpacaLF "rustic")
;;;; Haskell
;; (elpacaLF "haskell-mode")
;;;; Janet
(elpacaC "janet-ts-mode")
;;;; Clojure
(elpacaLF "clojure-mode")
;; (elpacaLF "clojure-ts-mode")
(elpacaLF "cider")
(elpacaLF "flymake-kondor")
;;;; LLVM
;; (elpacaLF "llvm-mode" t)
;;;; OCaml
(elpacaLF "tuareg")
(elpacaLF "merlin")
;;;; Zig
(elpacaLF "zig-mode")
;;;; PHP
(elpacaLF "php-mode")
;;;; Elixir
;;;;; REPL
(elpacaLF "inf-elixir")
;;;; CSV
(elpacaLF "csv-mode")

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
;; ;;;; Scroll bar for mode/headerline
;; (elpacaLF "mlscroll")
;; ;;;; RSS Reader
;; (elpacaLF "elfeed")
;; ;;;;; Org Integration
;; (elpacaLF "elfeed-org")
;;;; lexic (dictionary)
(elpacaLF "lexic")
;;;; scratch-pop
(elpacaLF "scratch-pop")
;;;; loccur
(elpacaLF "loccur")
;;;; Context-based cursor-line flash
;; (elpacaLF "beacon")
(elpacaLF "pulsar")
;;;; tiny (generate ranges)
(elpacaLF "tiny")
;;;; LLM
(elpacaC "ancilla")
;;;; Evil-Lispops
(elpacaLF "evil-lispops")
;;;; Breadcrumb
(elpacaLF "breadcrumb")
;;;; Legit
(elpacaLF "legit")
;;;; Whack-a-Thing
(elpacaLF "whack-a-thing")
;;;; Scratchiest
(elpacaLF "scratchiest")
;;;; Difflayer
(elpacaLF "difflayer")
;;;; Temper  Width
(elpacaLF "temper-width")
;; ;;;; MuCoCo
;; (elpacaLF "mucoco")

;;; Themes (that I never really use)
;; (elpacaLF "orangey-bits-theme") ;; Orange!
;; (elpacaLF "sakura-theme") ;; Purple!
;; (elpacaLF "sorcery-theme") ;; Inspo!
;; (elpacaLF "ef-themes") ;; Everything else!

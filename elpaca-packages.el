;; elpaca-packages.el -*- lexical-binding: t; -*-
(defmacro elpacaLF (package)
  `(let ((path (expand-file-name (concat "packages/" ,package ".el")
				user-emacs-directory)))
     (if (not (file-exists-p path))
       (progn
	 (message
	  (concat "Creating config file for package `"
		  ,package "'."))
	 (save-excursion
	   (write-region (concat
			  "(use-package "
			  ,package
			  ")")
			 nil
			 path))))
	 (load-file path)))

;;; Evil
(elpacaLF "evil")
(elpacaLF "evil-collection")
(elpacaLF "evil-exchange")
(elpacaLF "evil-lion")
(elpacaLF "evil-numbers")
(elpacaLF "evil-snipe")
(elpacaLF "evil-surround")
;;; Themes
;;;; Sculpture-Themes 
(elpaca sculpture-themes)
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
;;; Magit
(elpacaLF "magit")
;;; Undo
(elpacaLF "undo-tree")
;;; Olivetti
(elpacaLF "olivetti")


;;; Load Config File
;;(load-file (expand-file-name "package-config.el" user-emacs-directory))

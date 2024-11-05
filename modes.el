;;;; Tree-sitter

(defun treesit-verify-and-remap-major-mode (reset alist)
  "Verify a treesit grammar (TREESIT-GRAMMAR) exists, then add
MODE-FROM to MODE-TO in `major-mode-remap-alist’.  ALIST of
form (TREESIT-GRAMMAR MODE-FROM MODE-TO).  Set RESET to `t’ if
you want to `nil’ the alist before operation"
  (if reset (setq major-mode-remap-alist nil))
  (while (length> alist 0)
    (let* ((next (pop alist))
           (grammar (pop next))
           (from (pop next))
           (to (pop next)))
      (if (treesit-language-available-p grammar)
          (add-to-list 'major-mode-remap-alist (cons from to))))))

(treesit-verify-and-remap-major-mode t
 '((python python-mode python-ts-mode)
   (json js-json-mode json-ts-mode)
   (c c-mode c-ts-mode)
   (rust rust-mode rust-ts-mode)
   (haskell haskell-mode haskell-ts-mode)))

;;;; modes

(add-to-list 'auto-mode-alist (cons "\\.pde\\'" 'java-ts-mode))

;;;; ffap
(setq ffap-machine-p-known 'reject)

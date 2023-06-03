(use-package corfu
  :config
  (setq corfu-auto nil
        corfu-cycle t
        corfu-quit-no-match t)
  ;; (setq corfu-popupinfo-delay (cons nil 1.0))
  (setq corfu-separator ?\s)
  (define-key corfu-map (kbd "SPC")
              (lambda ()
                (interactive)
                (if current-prefix-arg
                    (progn 
                      (corfu-quit)
                      (insert " "))
                  (if (and (= (char-before) corfu-separator)
                           (or
                            (not (char-after))
                            (= (char-after) ?\s)
                            (= (char-after) ?\n)))
                      (progn
                        (corfu-insert)
                        (insert " "))
                    (corfu-insert-separator)))))
  (global-set-key (kbd "C-SPC") 'completion-at-point)
  :hook (prog-mode . corfu-mode))
  ;; (corfu-popupinfo-mode 1))

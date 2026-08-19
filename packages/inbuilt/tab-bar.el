(use-package tab-bar
  :ensure nil
  :config
  ;; (defface tab-bar-separator-face-c '((t :foreground "#FFF")) "Custom face for tab-bar separator.")
  (setq tab-bar-border nil
        tab-bar-button-margin 0
        tab-bar-button-relief 0
        tab-bar-auto-width nil
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-show 1
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-function #'tab-bar-tab-name-current
        tab-bar-forward-button ""
        tab-bar-back-button ""
        tab-bar-separator "")
  (defun tab-bar-tab-name-format-hints (name _tab i)
    "Re-defining the original.  Probably not a good idea.
Pad tab name on both sides.  It has effect when `tab-bar-tab-hints' is non-nil."
    (if tab-bar-tab-hints (concat (propertize "˚" 'face 'font-lock-keyword-face)
                                  (upcase name)
                                  (propertize " " 'face 'default))
      name))
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

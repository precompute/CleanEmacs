(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq server-client-instructions nil)
(setq frame-inhibit-implied-resize t)

(defface headerline-modified-active
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width (1 . 3) :color "#000")))
  "Headerline when buffer is modified and inactive.")
(defface headerline-unmodified-active
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width (1 . 3) :color "#000")))
  "Headerline when buffer is unmodified and inactive.")
(defface headerline-active-indicator
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for active window")
(defface headerline-inactive-modified-indicator
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for inactive, modified window")
(defface headerline-inactive-unmodified-indicator
  '((t :foreground "#000"
       :background "#000"))
  "Indicator for inactive, unmodified window")

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq server-client-instructions nil)
(setq frame-inhibit-implied-resize t)

(defface header-line--dummy
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width 1 :color "#000")))
  "Dummy header-line face")

(defface headerline-modified-inactive
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width 1 :color "#000")))
  "Headerline when buffer is modified and active.")
(defface headerline-modified-active
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width 1 :color "#000")))
  "Headerline when buffer is modified and inactive.")
(defface headerline-unmodified-inactive
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width 1 :color "#000")))
  "Headerline when buffer is unmodified and active.")
(defface headerline-unmodified-active
  '((t :foreground "#000"
       :background "#000"
       :box (:line-width 1 :color "#000")))
  "Headerline when buffer is unmodified and inactive.")

(use-package mailcap
  :ensure nil
  :config
  (dolist (z (list "mp4" "m4v" "wmv" "avi" "webm" "mkv" "gif" "mov" "mpeg"))
    (mailcap-add (concat "video/" z) "mpv --loop-file %s"))
  ;; (dolist (z (list "jpg" "jpeg" "jxl" "png" "webp"))
  ;;   (mailcap-add (concat "image/" z) "feh %s"))
  )

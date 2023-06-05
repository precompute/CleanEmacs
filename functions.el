;;; Functions
;;;; convenience
(defun consult-ripgrep-local ()
  (interactive)
  (let ((consult-project-function nil))
    (consult-ripgrep)))

(defun toggle-eldoc-box ()
  (interactive)
  (if (boundp 'toggle-eldoc-box--history)
      (progn
        (if (eq 1 (car toggle-eldoc-box--history))
            (funcall-interactively #'eldoc-box-hover-mode)
          (if (eq 1 (cdr toggle-eldoc-box--history))
              (funcall-interactively #'eldoc-box-hover-at-point-mode))))
    (defvar-local toggle-eldoc-box--history '(0 0))
    (let ((msg ""))
      (if eldoc-box-hover-mode
          (progn
            (setq-local toggle-eldoc-box--history
                        (list 1 (cdr toggle-eldoc-box--history)))
            (concat msg " `eldoc-box-hover-mode' disabled.")))
      (if eldoc-box-hover-at-point-mode
          (progn
            (setq-local toggle-eldoc-box--history
                        (list (car toggle-eldoc-box--history) 1))
            (concat msg " `eldoc-box-hover-at-point-mode' disabled.")))
      (message msg))))

;;;; completion
;;;;; kill-new from global paste
(defun kill-new-from-global-paste-c ()
  (interactive)
  (require 'sqlite3)
  (let ((mylist '()))
    (let* ((dbh (sqlite3-open "/home/sys2/.local/share/zeitgeist/activity.sqlite" sqlite-open-readonly))
           (stmt (sqlite3-prepare dbh "select * from text order by id desc limit 1000")))
      (while (= sqlite-row (sqlite3-step stmt))
        (cl-destructuring-bind (id text) (sqlite3-fetch stmt)
          (setq mylist (nconc mylist (list text)))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh))
    (kill-new
     (let* ((vertico-sort-function nil)
            (rlist (completing-read-multiple " " mylist))
            (rstr ""))
       (while (length> rlist 0)
         (setq rstr (concat rstr (pop rlist) "\n")))
       rstr))))

;;;; Shell Command Calls
;;;;; textmind stats
(defun textmind-stats-call ()
  (interactive)
  (async-shell-command "/home/sys2/44.1/python/textmindstats/textmindstats"))

;;;; Window Functions
;; It's like golden-ratio but just one function.
;; width, then height.
(defun make-window-larger-c (&optional ratio)
  "Makes the current window larger or smaller.
RATIO is the ratio used, default is 0.618.
It switches the width before the height."
  (interactive)
  (save-excursion
    (let* ((postv      (if olivetti-mode
                           (progn (olivetti-mode -1) t)
                         nil))
           (wwidth     (window-width))
           (wheight    (window-height))
           (fwidth     (frame-width))
           (fheight    (frame-height))
           (ratio      (if ratio ratio 0.618))
           (fl/fheight (floor (* ratio fheight)))
           (fl/fwidth  (floor (* ratio fwidth))))
      (if (window-full-width-p)
          (if (window-full-height-p) (message "Nothing to resize.")
            (if (>= wheight fl/fheight)
                (if (>= wwidth fl/fwidth)
                    (message "Nothing to resize.")
                  (enlarge-window (- fl/fwidth wwidth) t))
              (enlarge-window (- fl/fheight wheight))))
        (if (>= wwidth fl/fwidth)
            (if (>= wheight fl/fheight)
                (message "Nothing to resize.")
              (enlarge-window (- fl/fheight wheight)))
          (enlarge-window (- fl/fwidth wwidth) t)))
      (if postv (olivetti-mode 1)))))

(defun split-enlarge-follow-mode-toggle nil
  "Split window, enlarge left and then enable follow-mode."
  (interactive)
  (progn
    (call-interactively #'evil-window-vsplit)
    (call-interactively #'evil-window-left)
    (call-interactively #'follow-mode 1)))

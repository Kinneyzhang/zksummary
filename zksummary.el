(require 'zksummary-util)
(require 'zksummary-db)
(require 'zksummary-face)

(defvar zksummary-buffer "*zksummary*")
(defvar zksummary-default-type "daily")
(defvar zksummary-type-list '("daily" "weekly" "monthly" "yearly" "dwim"))
(defvar zksummary-capture-buffer "*Zksummary Capture*")

(defvar zksummary-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'zksummary-capture-finalize)
    (define-key map "\C-c\C-k" #'zksummary-capture-kill)
    map)
  "Keymap for `zksummary-capture-mode', a minor mode.")

;;;###autoload
(define-minor-mode zksummary-capture-mode
  "Minor mode for special key bindings in a capture buffer."
  :lighter " Cap"
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<zksummary-capture-mode-map>Capture buffer.  Finish \
`\\[zksummary-capture-finalize]', abort `\\[zksummary-capture-kill]'.")))

(defvar zksummary-window-configuration nil)
(defvar-local zksummary-capture-type nil)
(defvar-local zksummary-capture-time nil)
(defvar-local zksummary-capture-content nil)
(defvar-local zksummary-capture-id nil)

(defun zksummary-default-time-by-type (type)
  (pcase type
    ("daily" (format-time-string "%Y-%m-%d" (current-time)))
    ("monthly" (format-time-string "%Y-%m" (current-time)))
    ("yearly" (format-time-string "%Y" (current-time)))))

(defun zksummary-capture (&optional type time content id)
  (setq zksummary-window-configuration (current-window-configuration))
  (pop-to-buffer zksummary-capture-buffer)
  (setq zksummary-capture-type (or type zksummary-default-type))
  (setq zksummary-capture-time (or time (zksummary-default-time-by-type
                                         zksummary-capture-type)))
  (setq zksummary-capture-content content)
  (setq zksummary-capture-id id)
  (insert (format "%s summary: %s\n\n"
                  zksummary-capture-type zksummary-capture-time))
  (when content (insert content))
  (when zksummary-capture-id
    ;; make the meta info read only.
    (save-excursion
      (goto-char (point-min))
      (add-text-properties (line-beginning-position) (+ 2 (line-end-position)) '(read-only t))))
  (zksummary-capture-mode 1))

(defun zksummary-capture-finalize ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (type time content old-content old-id)
      (if (re-search-forward "^\\(.+\\) +summary: +\\([-0-9 ]+\\)"
                             (line-end-position) t)
          (progn
            (setq type (match-string-no-properties 1))
            (setq time (match-string-no-properties 2)))
        (user-error "Invalid format of summary meta!"))
      (forward-line 2)
      (setq content (buffer-substring-no-properties (point) (point-max)))
      (setq old-content zksummary-capture-content)
      (setq old-id zksummary-capture-id)
      (set-window-configuration zksummary-window-configuration)
      (kill-buffer zksummary-capture-buffer)
      (if old-id
          ;; edit a summary
          (unless (string= content old-content)
            (zksummary-ewoc-update old-id time type content)
            (zksummary-db-update old-id content))
        ;; add a new summary
        (let ((id (org-id-uuid)))
          (zksummary-ewoc-add id time type content)
          (zksummary-db-add type content time id))))))

(defun zksummary-capture-kill ()
  (interactive)
  (set-window-configuration zksummary-window-configuration)
  (kill-buffer zksummary-capture-buffer))

(defun zksummary-check-time (type time)
  "Check if TIME is a valid TYPE of time."
  t)

;; ;;;###autoload
;; (defun zksummary-add ()
;;   (interactive)
;;   (let ((type (completing-read "Choose a summary type: " zksummary-type-list nil t))
;;         (time (completing-read "Input a summary time: " nil)))
;;     (if (zksummary-check-time type time )
;;         (zksummary-capture type time)
;;       (user-error "[%] is an invalid time format of type [%s]" time type))))

(defvar zksummary-ewoc nil)
(defvar zksummary-current-type nil)
(defvar zksummary-current-time-range nil)

(defun zksummay-buffer-setup ()
  (erase-buffer)
  (kill-all-local-variables)
  (remove-overlays)
  (buffer-disable-undo)
  (setq major-mode 'zksummary-mode)
  (setq mode-name "zksummary")
  (use-local-map zksummary-mode-map))

(defun zksummary-ewoc-pp (data)
  (when data
    (let* ((id (nth 0 data))
           (summary-time (nth 1 data))
           (weekday
            (when (zksummary-is-date summary-time)
              (format-time-string
               "%a"
               (zksummary-date-to-seconds summary-time))))
           (type (nth 2 data))
           (content (nth 3 data)))
      (if weekday
          (insert (propertize (concat summary-time " " weekday)
                              'face 'zksummary-time-face))
        (insert (propertize summary-time
                            'face 'zksummary-time-face)))
      (insert "\n")
      (insert (propertize content 'id id) "\n\n"))))

;;;###autoload
(defun zksummary-show (&optional type timelst)
  "Show a type of zksummary notes, defaultly to `zksummary-default-type'.
If TIMELST is a list, it represents a list of time.
If TIMELST is a vector, it represents a range of time."
  (interactive)
  (let ((buf (get-buffer-create zksummary-buffer))
        (inhibit-read-only 1))
    (with-current-buffer buf
      (zksummay-buffer-setup)
      (setq-local zksummary-current-type type)
      ;; date/month/year list
      (setq-local zksummary-current-time-range timelst)
      (let* ((ewoc (ewoc-create 'zksummary-ewoc-pp nil nil t))
             (type (or type zksummary-default-type))
             (timelst (pcase timelst
                        ((pred consp) timelst)
                        ((pred vectorp) (zksummary-time-range-to-list timelst))))
             (summaries
              (if timelst
                  (zksummary-db-type-time-notes type (vconcat timelst))
                (zksummary-db-type-notes type))))
        (setq-local zksummary-ewoc ewoc)
        (dolist (summary summaries)
          (ewoc-enter-last ewoc summary)))
      (read-only-mode 1)
      (switch-to-buffer buf))))

;;;; daily view

;;;###autoload
(defun zksummary-daily-show ()
  (interactive)
  (zksummary-show "daily")
  (setq-local zksummary-view-type "daily-all"))

;;;###autoload
(defun zksummary-daily-show-curr-day ()
  (interactive)
  (zksummary-show "daily" (list (zksummary-seconds-to-date)))
  (setq-local zksummary-view-type "daily-date"))

;;;###autoload
(defun zksummary-daily-show-prev-day ()
  (interactive)
  (zksummary-show
   "daily"
   (list (zksummary-inc-db-date
          (nth 0 zksummary-current-time-range) -1)))
  (setq-local zksummary-view-type "daily-date"))

;;;###autoload
(defun zksummary-daily-show-next-day ()
  (interactive)
  (zksummary-show
   "daily"
   (list (zksummary-inc-db-date
          (nth 0 zksummary-current-time-range) 1)))
  (setq-local zksummary-view-type "daily-date"))

;;;###autoload
(defun zksummary-daily-show-curr-week ()
  (interactive)
  (zksummary-show "daily" (zksummary-week-date-lst))
  (setq-local zksummary-view-type "daily-week"))

;;;###autoload
(defun zksummary-daily-show-prev-week ()
  (interactive)
  (zksummary-show
   "daily" (zksummary-week-date-lst
            (zksummary-inc-date
             (nth 0 zksummary-current-time-range) -7)))
  (setq-local zksummary-view-type "daily-week"))

;;;###autoload
(defun zksummary-daily-show-next-week ()
  (interactive)
  (zksummary-show
   "daily" (zksummary-week-date-lst
            (zksummary-inc-date
             (nth 0 zksummary-current-time-range) 7)))
  (setq-local zksummary-view-type "daily-week"))

;;;###autoload
(defun zksummary-daily-show-curr-month ()
  (interactive)
  (zksummary-show "daily" (zksummary-month-date-lst))
  (setq-local zksummary-view-type "daily-month"))

;;;###autoload
(defun zksummary-daily-show-prev-month ()
  (interactive)
  (zksummary-show "daily" (zksummary-month-date-lst
                           (zksummary-inc-month
                            (substring (nth 0 zksummary-current-time-range) 0 7) -1)))
  (setq-local zksummary-view-type "daily-month"))

;;;###autoload
(defun zksummary-daily-show-next-month ()
  (interactive)
  (zksummary-show "daily" (zksummary-month-date-lst
                           (zksummary-inc-month
                            (substring (nth 0 zksummary-current-time-range) 0 7) 1)))
  (setq-local zksummary-view-type "daily-month"))

;;;; weekly view
;;;###autoload
(defun zksummary-weekly-show ()
  (interactive)
  (zksummary-show "weekly")
  (setq-local zksummary-view-type "weekly-all"))

;; (defun zksummary-weekly-show-curr-week ()
;;   (interactive)
;;   (zksummary-show "weekly")
;;   (setq-local zksummary-view-type "weekly-week"))

;; (defun zksummary-weekly-show-prev-week ()
;;   (interactive)
;;   (zksummary-show "weekly")
;;   (setq-local zksummary-view-type "weekly-week"))

;; (defun zksummary-weekly-show-next-week ()
;;   (interactive)
;;   (zksummary-show "weekly")
;;   (setq-local zksummary-view-type "weekly-week"))

;;; monthly view
;;;###autoload
(defun zksummary-monthly-show ()
  (interactive)
  (zksummary-show "monthly")
  (setq-local zksummary-view-type "monthly-all"))

;;;###autoload
(defun zksummary-yearly-show ()
  (interactive)
  (zksummary-show "yearly")
  (setq-local zksummary-view-type "yearly-all"))

;;;###autoload
(defun zksummary-add ()
  (interactive)
  (zksummary-capture zksummary-current-type))

;;;###autoload
(defun zksummary-delete ()
  (interactive)
  (let ((id (zksummary-ewoc-data :id)))
    (when (y-or-n-p "Delete current summary?")
      (zksummary-ewoc-delete)
      (zksummary-db-delete id))))

;;;###autoload
(defun zksummary-edit ()
  (interactive)
  (let ((type (zksummary-ewoc-data :type))
        (id (zksummary-ewoc-data :id))
        (time (zksummary-ewoc-data :time))
        (content (zksummary-ewoc-data :content)))
    (zksummary-capture type time content id)))

;;;###autoload
(defun zksummary-refresh ()
  (interactive)
  (zksummary-show zksummary-current-type))

;;;###autoload
(defun zksummary-quit ()
  (interactive)
  (kill-buffer zksummary-buffer)
  (zksummary-db--close)
  (quit-window))

(defvar zksummary-view-type nil
  "daily-date, daily-week, daily-month, daily-year, daily-all,
   weekly-week, weekly-all,
   monthly-month, monthly-year, monthly-all")

(defvar zksummary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" #'zksummary-daily-show)
    (define-key map "W" #'zksummary-weekly-show)
    (define-key map "M" #'zksummary-monthly-show)
    (define-key map "Y" #'zksummary-yearly-show)
    
    (define-key map "d" #'zksummary-view-show-curr)
    (define-key map "b" #'zksummary-view-show-prev)
    (define-key map "f" #'zksummary-view-show-next)
    
    (define-key map "a" #'zksummary-add)
    (define-key map "td" #'zksummary-delete)
    (define-key map "g" #'zksummary-refresh)
    (define-key map "q" #'zksummary-quit)
    (define-key map "e" #'zksummary-edit)    
    map))

(defun zksummary-view-show-curr ()
  (interactive)
  (pcase zksummary-view-type
    ("daily-date" (zksummary-daily-show-curr-day))
    ("daily-week" (zksummary-daily-show-curr-week))
    ("daily-month" (zksummary-daily-show-curr-monthly))))

(defun zksummary-view-show-prev ()
  (interactive)
  (pcase zksummary-view-type
    ("daily-date" (zksummary-daily-show-prev-day))
    ("daily-week" (zksummary-daily-show-prev-week))
    ("daily-month" (zksummary-daily-show-prev-month))))

(defun zksummary-view-show-next ()
  (interactive)
  (pcase zksummary-view-type
    ("daily-date" (zksummary-daily-show-next-day))
    ("daily-week" (zksummary-daily-show-next-week))
    ("daily-month" (zksummary-daily-show-next-month))))

(provide 'zksummary)

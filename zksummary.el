(require 'zksummary-util)
(require 'zksummary-db)
(require 'zksummary-face)
(require 'valign)

(defvar zksummary-buffer "*zksummary*")
(defvar zksummary-default-type "daily")
(defvar zksummary-type-list '("daily" "weekly" "monthly" "yearly" "dwim"))
(defvar zksummary-capture-buffer "*Zksummary Capture*")

(defvar zksummary-window-width 55)

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
    ("daily" (zksummary-date-str))
    ("weekly" (zksummary-week-str))
    ("monthly" (zksummary-month-str))
    ("yearly" (zksummary-year-str))))

(defun zksummary-capture--at-content-start ()
  (let ((content-start (save-excursion
                         (goto-char (point-min))
                         (+ 2 (line-end-position)))))
    (= (point) content-start)))

(defun zksummary-capture (&optional type time content id)
  (setq zksummary-window-configuration (current-window-configuration))
  (switch-to-buffer zksummary-capture-buffer)
  (orgtbl-mode 1)
  ;; (org-mode)
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
      (add-text-properties (line-beginning-position)
                           (+ 1 (line-end-position))
                           '(read-only t))))
  (valign-mode 1)
  (zksummary-valign-table)
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
          (zksummary-db-add type content time id)))
      (zksummary-valign-table))))

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
(defvar zksummary-entry-type nil)
(defvar zksummary-curr-timeseq nil)

(defun zksummay-buffer-setup ()
  (erase-buffer)
  (kill-all-local-variables)
  (remove-overlays)
  (buffer-disable-undo)
  (setq major-mode 'zksummary-mode)
  (setq mode-name "zksummary")
  (use-local-map zksummary-mode-map))

(defun zksummary-time-shown (summary-time)
  (pcase summary-time
    ((pred zksummary-is-date)
     (concat summary-time " " (zksummary-day-of-week summary-time)))
    ((pred zksummary-is-week)
     (string-join (split-string summary-time " " t) " ~ "))
    (_ summary-time)))

(defun zksummary-ewoc-pp (data)
  (if data
      (let* ((id (nth 0 data))
             (summary-time (nth 1 data))
             (type (nth 2 data))
             (content (nth 3 data)))
        (insert (propertize (zksummary-time-shown summary-time)
                            'face 'zksummary-time-face))
        (insert "\n")
        (insert (propertize content 'id id) "\n\n"))
    (insert "No entries yet!")))

(defun zksummary-view-title (type period)
  (format "%s Summaries For A %s\n"
          (upcase-initials type)
          (upcase-initials period)))

(defun zksummary-view-keystr ()
  "[f]:view forward [b]:view backward [.]:view current")

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
      (let* ((type-lst (split-string type "-" t))
             (entry-type (car type-lst))
             (entry-period (cadr type-lst))
             (title (propertize (zksummary-view-title entry-type entry-period)
                                'face 'zksummary-title-face))
             (keyhint (propertize (zksummary-view-keystr)
                                  'face 'zksummary-keyhint-face))
             (ewoc-header (concat "\n" title "\n")))
        (setq-local header-line-format keyhint)
        (setq-local zksummary-view-type type)
        (setq-local zksummary-entry-type entry-type)
        (setq-local zksummary-curr-timeseq timelst)
        (let* ((ewoc (ewoc-create 'zksummary-ewoc-pp ewoc-header nil t))
               (type (or zksummary-entry-type zksummary-default-type))
               (timelst (pcase timelst
                          ((pred consp) timelst)
                          ((pred vectorp)
                           (zksummary-time-range-to-list timelst))))
               (summaries
                (if timelst
                    (zksummary-db-type-time-entries type (vconcat timelst))
                  (zksummary-db-type-entries type))))
          (setq-local zksummary-ewoc ewoc)
          (if (null summaries)
              (ewoc-enter-last ewoc nil)
            (dolist (summary summaries)
              (ewoc-enter-last ewoc summary)))))
      (zksummary-valign-table)
      (read-only-mode 1))
    (switch-to-buffer buf)
    ;; (if-let ((_ (select-window-by-buffer zksummary-buffer)))
    ;;     (ignore)
    ;;   (when zksummary-window-width
    ;;     (split-window-horizontally zksummary-window-width))
    ;;   (switch-to-buffer buf))
    ))

(defun zksummary-valign-table ()
  (save-excursion
    (while (< (point) (point-max))
      (if (valign--at-table-p)
          (progn
            (valign-table)
            (valign--end-of-table)
            (forward-line 1))
        (forward-line 1)))))

;;;; daily view

;;;###autoload
(defun zksummary-daily-show ()
  (interactive)
  (zksummary-show "daily-all"))

;;;###autoload
(defun zksummary-daily-show-now-date ()
  (interactive)
  (zksummary-show
   "daily-date" (list (zksummary-seconds-to-date))))

;;;###autoload
(defun zksummary-daily-show-curr-date ()
  (interactive)
  (zksummary-show
   "daily-date" (list (nth 0 zksummary-curr-timeseq))))

;;;###autoload
(defun zksummary-daily-show-prev-date ()
  (interactive)
  (zksummary-show
   "daily-date"
   (list (zksummary-inc-db-date
          (nth 0 zksummary-curr-timeseq) -1))))

;;;###autoload
(defun zksummary-daily-show-next-date ()
  (interactive)
  (zksummary-show
   "daily-date"
   (list (zksummary-inc-db-date
          (nth 0 zksummary-curr-timeseq) 1))))

;;;###autoload
(defun zksummary-daily-show-now-week ()
  (interactive)
  (zksummary-show "daily-week" (zksummary-week-date-lst)))

;;;###autoload
(defun zksummary-daily-show-curr-week ()
  (interactive)
  (zksummary-show "daily-week"
                  (zksummary-week-date-lst
                   (nth 0 zksummary-curr-timeseq))))

;;;###autoload
(defun zksummary-daily-show-prev-week ()
  (interactive)
  (zksummary-show
   "daily-week" (zksummary-inc-db-week-date-lst
                 zksummary-curr-timeseq -1)))

;;;###autoload
(defun zksummary-daily-show-next-week ()
  (interactive)
  (zksummary-show
   "daily-week" (zksummary-inc-db-week-date-lst
                 zksummary-curr-timeseq 1)))

;; show even not exisit entry in db
;; don't show if not exisit entry in db

;;;###autoload
(defun zksummary-daily-show-now-month ()
  (interactive)
  (zksummary-show "daily-month" (zksummary-month-date-lst)))

;;;###autoload
(defun zksummary-daily-show-curr-month ()
  (interactive)
  (zksummary-show "daily-month"
                  (zksummary-month-date-lst
                   (zksummary-month-str
                    (nth 0 zksummary-curr-timeseq)))))

;;;###autoload
(defun zksummary-daily-show-prev-month ()
  (interactive)
  (zksummary-show
   "daily-month"
   (zksummary-month-date-lst
    (zksummary-daily-inc-db-month
     (zksummary-month-str (nth 0 zksummary-curr-timeseq)) -1))))

;;;###autoload
(defun zksummary-daily-show-next-month ()
  (interactive)
  (zksummary-show
   "daily-month"
   (zksummary-month-date-lst
    (zksummary-daily-inc-db-month
     (zksummary-month-str (nth 0 zksummary-curr-timeseq)) 1))))

;;;; weekly view
;;;###autoload
(defun zksummary-weekly-show ()
  (interactive)
  (zksummary-show "weekly-all"))

(defun zksummary-weekly-show-now-week ()
  (interactive)
  (zksummary-show "weekly-week"
                  (list (zksummary-week-str))))

(defun zksummary-weekly-show-curr-week ()
  ;; 支持 当前日期所在的 weekly summary
  (interactive)
  (zksummary-show "weekly-week"
                  (list (zksummary-week-str
                         (nth 0 zksummary-curr-timeseq)))))

(defun zksummary-weekly-show-prev-week ()
  (interactive)
  (zksummary-show "weekly-week"
                  (list (zksummary-inc-db-week
                         (nth 0 zksummary-curr-timeseq) -1))))

(defun zksummary-weekly-show-next-week ()
  (interactive)
  (zksummary-show "weekly-week"
                  (list (zksummary-inc-db-week
                         (nth 0 zksummary-curr-timeseq) 1))))

;;; monthly view
;;;###autoload
(defun zksummary-monthly-show ()
  (interactive)
  (zksummary-show "monthly-all"))

;;;###autoload
(defun zksummary-monthly-show-now-month ()
  (interactive)
  (zksummary-show "monthly-month"
                  (list (zksummary-month-str-dwim))))

;;;###autoload
(defun zksummary-monthly-show-curr-month ()
  (interactive)
  (zksummary-show "monthly-month"
                  (list (zksummary-month-str-dwim
                         (nth 0 zksummary-curr-timeseq)))))

;;;###autoload
(defun zksummary-monthly-show-prev-month ()
  (interactive)
  (zksummary-show
   "monthly-month"
   (list (zksummary-inc-db-month
          (nth 0 zksummary-curr-timeseq) -1))))

;;;###autoload
(defun zksummary-monthly-show-next-month ()
  (interactive)
  (zksummary-show
   "monthly-month"
   (list (zksummary-inc-db-month
          (nth 0 zksummary-curr-timeseq) 1))))

;;; yearly show
;;;###autoload
(defun zksummary-yearly-show ()
  (interactive)
  (zksummary-show "yearly-all"))

;;;###autoload
(defun zksummary-yearly-show-now-year ()
  (interactive)
  (zksummary-show "yearly-year"
                  (list (zksummary-year-str))))

;;;###autoload
(defun zksummary-yearly-show-curr-year ()
  (interactive)
  (zksummary-show "yearly-year"
                  (list (zksummary-year-str-dwim
                         (nth 0 zksummary-curr-timeseq)))))

;;;###autoload
(defun zksummary-yearly-show-prev-year ()
  (interactive)
  (zksummary-show
   "yearly-year"
   (list (zksummary-inc-db-year
          (nth 0 zksummary-curr-timeseq) -1))))

;;;###autoload
(defun zksummary-yearly-show-next-year ()
  (interactive)
  (zksummary-show
   "yearly-year"
   (list (zksummary-inc-db-year
          (nth 0 zksummary-curr-timeseq) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun zksummary-add ()
  (interactive)
  (zksummary-capture zksummary-entry-type))

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
  (zksummary-show zksummary-view-type zksummary-curr-timeseq))

;;;###autoload
(defun zksummary-quit ()
  (interactive)
  (kill-buffer zksummary-buffer)
  (zksummary-db--close)
  (quit-window))

(defvar zksummary-view-type nil
  "daily-date, daily-week, daily-month, daily-year, daily-all,
   weekly-week, weekly-all,
   monthly-month, monthly-year, monthly-all,
   yearly-all.")

(defun zksummary-view-show-now ()
  (interactive)
  (pcase zksummary-view-type
    ("daily-date" (zksummary-daily-show-now-date))
    ("daily-week" (zksummary-daily-show-now-week))
    ("daily-month" (zksummary-daily-show-now-monthly))
    ("weekly-week" (zksummary-weekly-show-now-week))
    ("monthly-month" (zksummary-monthly-show-now-month))
    ("yearly-year" (zksummary-yearly-show-now-year))))

(defun zksummary-view-show-prev ()
  (interactive)
  (pcase zksummary-view-type
    ("daily-date" (zksummary-daily-show-prev-date))
    ("daily-week" (zksummary-daily-show-prev-week))
    ("daily-month" (zksummary-daily-show-prev-month))
    ("weekly-week" (zksummary-weekly-show-prev-week))
    ("monthly-month" (zksummary-monthly-show-prev-month))
    ("yearly-year" (zksummary-yearly-show-prev-year))))

(defun zksummary-view-show-next ()
  (interactive)
  (pcase zksummary-view-type
    ("daily-date" (zksummary-daily-show-next-date))
    ("daily-week" (zksummary-daily-show-next-week))
    ("daily-month" (zksummary-daily-show-next-month))
    ("weekly-week" (zksummary-weekly-show-next-week))
    ("monthly-month" (zksummary-monthly-show-next-month))
    ("yearly-year" (zksummary-yearly-show-next-year))))

(defun ewoc-last-node-pos (ewoc)
  (ewoc--set-buffer-bind-dll ewoc
                             (ewoc--node-start-marker
                              ;; here last node is not the footer node
                              (ewoc--node-nth dll -2))))

(defun ewoc-first-node-pos (ewoc)
  (ewoc--set-buffer-bind-dll ewoc
                             (ewoc--node-start-marker
                              ;; here first node is not the header node
                              (ewoc--node-nth dll 1))))

(defun ewoc-node-pos (ewoc)
  (ewoc-location (ewoc-locate ewoc)))

(defun zksummary-goto-next ()
  (interactive)
  (unless (= (ewoc-last-node-pos zksummary-ewoc)
             (ewoc-node-pos zksummary-ewoc))
    (ignore (ewoc-goto-next zksummary-ewoc 1))
    (recenter-top-bottom 0)))

(defun zksummary-goto-prev ()
  (interactive)
  (ignore (ewoc-goto-prev zksummary-ewoc 1))
  (if (= (ewoc-first-node-pos zksummary-ewoc)
         (ewoc-node-pos zksummary-ewoc))
      (recenter-top-bottom -1)
    (recenter-top-bottom 0)))

(defvar zksummary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "dd" #'zksummary-daily-show-now-date)
    (define-key map "dw" #'zksummary-daily-show-now-week)
    (define-key map "dm" #'zksummary-daily-show-now-month)
    (define-key map "ww" #'zksummary-weekly-show-now-week)
    (define-key map "mm" #'zksummary-monthly-show-now-month)
    (define-key map "yy" #'zksummary-yearly-show-now-year)
    
    (define-key map "D" #'zksummary-daily-show-curr-date)
    (define-key map "W" #'zksummary-weekly-show-curr-week)
    (define-key map "M" #'zksummary-monthly-show-curr-month)
    (define-key map "Y" #'zksummary-yearly-show)
    
    (define-key map "." #'zksummary-view-show-now)
    (define-key map "b" #'zksummary-view-show-prev)
    (define-key map "f" #'zksummary-view-show-next)
    
    (define-key map "a" #'zksummary-add)
    (define-key map "td" #'zksummary-delete)
    (define-key map "g" #'zksummary-refresh)
    (define-key map "q" #'zksummary-quit)
    (define-key map "e" #'zksummary-edit)

    (define-key map "p" #'zksummary-goto-prev)
    (define-key map "n" #'zksummary-goto-next)
    map))

(provide 'zksummary)

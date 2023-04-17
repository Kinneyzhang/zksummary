;; FIXME: use some api
(defun most-frequent-string (string-list)
  "Return the most frequent string in STRING-LIST."
  (let ((string-counts (make-hash-table :test 'equal)))
    ;; Count the occurrences of each string
    (dolist (string string-list)
      (let ((count (gethash string string-counts 0)))
        (puthash string (1+ count) string-counts)))
    ;; Find the string with the highest count
    (let ((max-count 0)
          (max-string nil))
      (maphash (lambda (string count)
                 (when (> count max-count)
                   (setq max-count count
                         max-string string)))
               string-counts)
      max-string)))

(defun zksummary-week-month (week-str)
  (let* ((from-date
          (nth 0 (split-string week-str " " t " +")))
         (date-lst (zksummary-week-date-lst from-date)))
    (most-frequent-string
     (mapcar #'zksummary-month-str date-lst))))

(defun zksummary-week-year (week-str)
  (let* ((from-date
          (nth 0 (split-string week-str " " t " +")))
         (date-lst (zksummary-week-date-lst from-date)))
    (most-frequent-string
     (mapcar #'zksummary-year-str date-lst))))

(defun zksummary-month-str-dwim (&optional time)
  ;; date -> curr month
  ;; week -> has more days in a month
  ;; month -> curr month
  ;; year -> now month
  (pcase time
    ((pred null) (zksummary-month-str))
    ((pred zksummary-is-date)
     (zksummary-month-str time))
    ((pred zksummary-is-week)
     (zksummary-week-month time))
    ((pred zksummary-is-month) time)
    ((pred zksummary-is-year)
     (concat time "-" "12"))))

(defun zksummary-week-str-dwim (&optional time)
  ;; date -> curr week
  ;; week -> curr week
  ;; month -> latest week in month
  ;; year -> now week
  )

(defun zksummary-year-str-dwim (&optional time)
  ;; date -> curr year
  ;; week -> has more days in a year
  ;; month -> curr year
  ;; year -> curr year
  (pcase time
    ((pred null) (zksummary-year-str))
    ((pred zksummary-is-date)
     (zksummary-year-str time))
    ((pred zksummary-is-week)
     (zksummary-week-year time))
    ((pred zksummary-is-month) (substring time 0 4))
    ((pred zksummary-is-year) time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-window-by-buffer (buffer-name)
  (let ((buf-win-alist
         (mapcar (lambda (win)
                   (cons (buffer-name (window-buffer win)) win))
                 (window-list))))
    (if-let ((pair (assoc buffer-name buf-win-alist)))
        (select-window (cdr pair)))))

(defun zksummary-buffer-p ()
  (string= (buffer-name) zksummary-buffer))

(defun zksummary-day-of-week (&optional date)
  (format-time-string "%a" (zksummary-date-to-seconds
                            (zksummary-date-str date))))

(defun zksummary-is-date (time)
  (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" time))

(defun zksummary-is-week (time)
  (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" time))

(defun zksummary-is-month (time)
  (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}$" time))

(defun zksummary-is-year (time)
  (string-match "^[0-9]\\{4\\}$" time))

(defun zksummary-date-str (&optional date)
  (or date (zksummary-seconds-to-date)))

(defun zksummary-week-str (&optional date)
  (let* ((day-lst (zksummary-week-date-lst date))
         (Mon-date (car day-lst))
         (Sun-date (car (last day-lst))))
    (concat Mon-date " " Sun-date)))

(defun zksummary-month-str (&optional date)
  (substring (zksummary-date-str date) 0 7))

(defun zksummary-year-str (&optional date)
  (substring (zksummary-date-str date) 0 4))

(defun zksummary-date-to-seconds (&optional date)
  "Convert date to seconds."
  (let ((date (or date (zksummary-seconds-to-date))))
    (time-convert (date-to-time (concat date " 00:00:00")) 'integer)))

(defun zksummary-seconds-to-date (&optional seconds)
  "Convert seconds to a date."
  (format-time-string "%Y-%m-%d" seconds))

(defun zksummary-inc-week (week n)
  "Week is a string of two dates which are Monday and Sunday."
  (let* ((lst (split-string week " " t))
         (from-date (car lst))
         (to-date (cadr lst))
         (new-from-date (zksummary-inc-date from-date (* n 7)))
         (new-to-date (zksummary-inc-date to-date (* n 7))))
    (concat new-from-date " " new-to-date)))

(defun zksummary-inc-db-week (week n)
  (let* ((max-week (zksummary-db-max-time "weekly"))
         (min-week (zksummary-db-min-time "weekly"))
         (new-week (zksummary-inc-week week n)))
    (pcase nil
      ((and (guard (string> new-week max-week))
            (guard (> n 0)))
       (message "The lastest week summary!")
       (setq new-week week))
      ((and (guard (string< new-week min-week))
            (guard (< n 0)))
       (message "The earlist week summary!")
       (setq new-week week))
      (_ (while (= (zksummary-db-count-by-time new-week) 0)
           (setq new-week (zksummary-inc-week new-week n)))))
    new-week))

(defun zksummary-fmt-month-str (n)
  (if (< n 10)
      (format "0%s" n)
    (number-to-string n)))

(defun zksummary-inc-month (month n)
  (let* ((y (substring month 0 4))
         (m (string-to-number (substring month 5 7)))
         (s (+ m n)))
    (pcase n
      ((pred (= 0)) month)
      ((pred (< 0))
       (if (<= s 12)
           (concat y "-" (zksummary-fmt-month-str s))
         (let* ((y-inc (/ s 12))
                (new-m (% s 12))
                (new-y (+ (string-to-number y) y-inc)))
           (concat (number-to-string new-y)
                   "-" (zksummary-fmt-month-str new-m)))))
      ((pred (> 0))
       (if (> s 0)
           (concat y "-" (zksummary-fmt-month-str s))
         (let* ((y-inc (1- (/ s 12)))
                (new-y (+ (string-to-number y) y-inc))
                (new-m (+ 12 (% s 12))))
           (concat (number-to-string new-y)
                   "-" (zksummary-fmt-month-str new-m))))))))

(defun zksummary-inc-db-month (month n)
  (let ((max-month (zksummary-db-max-time "monthly"))
        (min-month (zksummary-db-min-time "monthly"))
        (new-month (zksummary-inc-month month n)))
    (pcase nil
      ((and (guard (string> new-month max-month))
            (guard (> n 0)))
       (message "The lastest month summary!")
       (setq new-month month))
      ((and (guard (string< new-month min-month))
            (guard (< n 0)))
       (message "The earlist month summary!")
       (setq new-month month))
      (_ (while (= (zksummary-db-count-by-time new-month) 0)
           (setq new-month (zksummary-inc-month new-month n)))))
    new-month))

(defun zksummary-daily-inc-db-month (month n)
  (let ((max-month (zksummary-month-str
                    (zksummary-db-max-time "daily")))
        (min-month (zksummary-month-str
                    (zksummary-db-min-time "daily")))
        (new-month (zksummary-inc-month month n)))
    (pcase new-month
      ((and (pred (string< max-month))
            (guard (> n 0)))
       (message "The latest daily-month summary!")
       (setq new-month month))
      ((and (pred (string> min-month))
            (guard (< n 0)))
       (message "The earlist daily-month summary!")
       (setq new-month month))
      ;; all date in a month has no summary, skip
      (_ (while (= 0 (seq-reduce
                      '+ (mapcar #'zksummary-db-count-by-time
                                 (zksummary-month-date-lst "2023-04"))
                      0))
           (setq new-month (zksummary-inc-month month n)))))
    new-month))

(defun zksummary-inc-year (year n)
  (number-to-string (+ (string-to-number year) n)))

(defun zksummary-inc-db-year (year n)
  (let ((max-year (zksummary-db-max-time "yearly"))
        (min-year (zksummary-db-min-time "yearly"))
        (new-year (zksummary-inc-year year n)))
    (pcase new-year
      ((and (pred (string< max-year))
            (guard (> n 0)))
       (message "The latest yearly summary!")
       (setq new-year year))
      ((and (pred (string> min-year))
            (guard (< n 0)))
       (message "The earlist yearly summary!")
       (setq new-year year))
      (_ (while (= (zksummary-db-count-by-time new-year) 0)
           (setq new-year (zksummary-inc-year new-year n)))))
    new-year))

(defun zksummary-inc-date (date n)
  (let* ((second (zksummary-date-to-seconds date))
         (new-second (+ second (* n (* 24 60 60)))))
    (zksummary-seconds-to-date new-second)))

(defun zksummary-inc-db-date (date n)
  "Increate N days of DATE on which must have a record in db."
  (let ((max-date (zksummary-db-max-time "daily"))
        (min-date (zksummary-db-min-time "daily"))
        (new-date (zksummary-inc-date date n)))
    (pcase new-date
      ((and (pred (string< max-date))
            (guard (> n 0)))
       (message "The latest daily summary!")
       (setq new-date date))
      ((and (pred (string> min-date))
            (guard (< n 0)))
       (message "The earlist daily summary!")
       (setq new-date date))
      (_ (while (= (zksummary-db-count-by-time new-date) 0)
           (setq new-date (zksummary-inc-date new-date n)))))
    new-date))

(defun zksummary-daily-week-check (week-date-lst n)
  ;; check if the first day of week is later than the db max time.
  ;; check if the last day of week is ealier that the db min time.
  (let ((max (zksummary-db-max-time "daily"))
        (min (zksummary-db-min-time "daily"))
        (first (car week-date-lst))
        (last (car (last week-date-lst))))
    (pcase nil
      ((and (guard (not (string< last max)))
            (guard (> n 0)))
       "The latest daily-week summary!")
      ((and (guard (not (string> first min)))
            (guard (< n 0)))
       "The earlist daily-week summary!")
      (_ nil))))

(defun zksummary-inc-week-date-lst (week-date-lst n)
  (zksummary-week-date-lst
   (zksummary-inc-date
    (nth 0 week-date-lst) (* n 7))))

;; FIX: modify `zksummary-inc-db-date' to make it more common!
;; all date of week has no summary, skip!
(defun zksummary-inc-db-week-date-lst (week-date-lst n)
  (let* ((lst week-date-lst)
         (res (zksummary-daily-week-check lst n)))
    (pcase res
      ((pred stringp)
       (message "%s" res))
      ((pred null)
       (setq lst (zksummary-inc-week-date-lst lst n))
       (while (null (zksummary-db-type-time-entries "daily"
                                                    (vconcat lst)))
         (setq lst (zksummary-inc-week-date-lst lst n)))))
    lst))

(defun zksummary-calendar-date (&optional seconds)
  (list (string-to-number (format-time-string "%m" seconds))
        (string-to-number (format-time-string "%d" seconds))
        (string-to-number (format-time-string "%Y" seconds))))

(defun zksummary--weekdays-list (first-day-second)
  (let ((seccond-lst))
    (dotimes (i 7)
      (setq seccond-lst
            (append seccond-lst (list (+ first-day-second
                                         (* i (* 24 60 60)))))))
    (mapcar #'zksummary-seconds-to-date seccond-lst)))

(defun zksummary-week-date-lst (&optional date)
  "A list of all date in a week. DATE is a day of the week."
  (let* ((day-seconds (zksummary-date-to-seconds date))
         (calendar-date (zksummary-calendar-date day-seconds))
         (nth (calendar-day-of-week calendar-date))
         (minused-days (let ((days (1- nth)))
                         (if (< days 0) (+ days 7) days)))
         (minused-seconds (* minused-days (* 24 60 60)))
         (first-weekday-seconds (- day-seconds minused-seconds)))
    (zksummary--weekdays-list first-weekday-seconds)))

(defun zksummary-month-date-lst (&optional month)
  "A list of all date in a month. DATE is a day of the month."
  (let* ((month-str (or month
                        (zksummary-month-str)))
         (year (string-to-number (substring month-str 0 4)))
         (month (string-to-number (substring month-str 5 7)))
         (days (date-days-in-month year month)))
    (mapcar (lambda (num)
              (concat month-str "-" (zksummary-fmt-month-str num)))
            (number-sequence 1 days 1))))

(defun zksummary-floor-time (time)
  (let* ((timelst (zksummary-ewoc-buffer-data :time))
         (sorted-timelst (sort timelst #'string>)))
    (seq-find (lambda (el)
                (string< el time))
              sorted-timelst)))

(defun zksummary-time-range-to-list (timelst)
  (let ((from (aref timelst 0))
        (to (aref timelst 1)))
    (pcase from
      ((pred zksummary-is-date) (zksummary-date-seq from to))
      ((pred zksummary-is-month) (zksummary-month-seq from to))
      ((pred zksummary-is-year) (zksummary-year-seq from to))
      (_ (error "Wrong format of zksummary time: %s" from)))))

(defun zksummary-date-seq (from to)
  (if (> (zksummary-date-to-seconds from)
         (zksummary-date-to-seconds to))
      (error "FROM date should not later than TO date!")
    (let ((lst))
      (while (<= (zksummary-date-to-seconds from)
                 (zksummary-date-to-seconds to))
        (setq lst (append lst (list from)))
        (setq from (zksummary-inc-date from 1)))
      lst)))

(defun zksummary-month-seq (from to)
  (if (string> from to)
      (error "FROM month should not later than TO month!")
    (let ((lst))
      (while (not (string> from to))
        (setq lst (append lst (list from)))
        (setq from (zksummary-inc-month from 1)))
      lst)))

(defun zksummary-year-seq (from to)
  (if (string> from to)
      (error "FROM year should not later than TO year!")
    (mapcar #'number-to-string
            (number-sequence (string-to-number from) (string-to-number to) 1))))

(defun zksummary-ewoc-node (&rest kv)
  ;; Return the ewoc node matched by kv.
  (let ((data (zksummary-ewoc-buffer-data)))
    (let* ((k (nth 0 kv))
           (v (nth 1 kv))
           (v-seq (zksummary-ewoc-buffer-data k)))
      (ewoc-nth zksummary-ewoc (seq-position v-seq v)))))

(defun zksummary-ewoc-buffer-data (&optional keyword)
  (let ((data (ewoc-collect zksummary-ewoc #'consp)))
    (if keyword
        (pcase keyword
          (:id (mapcar (lambda (lst) (nth 0 lst)) data))
          (:time (mapcar (lambda (lst) (nth 1 lst)) data))
          (:type (mapcar (lambda (lst) (nth 2 lst)) data))
          (:content (mapcar (lambda (lst) (nth 3 lst)) data)))
      data)))

(defun zksummary-ewoc-data (&optional keyword)
  "Return ewoc data at current position."
  (let ((data (ewoc-data (ewoc-locate zksummary-ewoc))))
    (if keyword
        (pcase keyword
          (:id (nth 0 data))
          (:time (nth 1 data))
          (:type (nth 2 data))
          (:content (nth 3 data)))
      data)))

(defun zksummary-ewoc-delete ()
  (let ((inhibit-read-only t))
    (ewoc-delete zksummary-ewoc (ewoc-locate zksummary-ewoc))))

(defun zksummary-ewoc-add (id time type content)
  (with-current-buffer (get-buffer zksummary-buffer)
    (let* ((inhibit-read-only t)
           (data (list id time type content))
           (floor-time (zksummary-floor-time time))
           node)
      (if floor-time
          (progn (setq node (zksummary-ewoc-node :time floor-time))
                 (ewoc-enter-before zksummary-ewoc node data))
        ;; after the last node
        (ewoc-enter-last zksummary-ewoc data)))))

(defun zksummary-ewoc-update (id time type content)
  (with-current-buffer (get-buffer zksummary-buffer)
    (let ((node (zksummary-ewoc-node :id id)))
      (ewoc-set-data node (list id time type content))
      (ewoc-invalidate zksummary-ewoc node))))

(provide 'zksummary-util)

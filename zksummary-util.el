(defun zksummary-date-to-seconds (&optional date)
  "Convert date to seconds."
  (let ((date (or date (zksummary-seconds-to-date))))
    (time-convert (date-to-time (concat date " 00:00:00")) 'integer)))

(defun zksummary-seconds-to-date (&optional seconds)
  "Convert seconds to a date."
  (format-time-string "%Y-%m-%d" seconds))

(defun zksummary-inc-date (date n)
  (let* ((second (zksummary-date-to-seconds date))
         (new-second (+ second (* n (* 24 60 60)))))
    (zksummary-seconds-to-date new-second)))

(defun zksummary-inc-db-date (date n)
  "Increate N days of DATE on which must have a record in db."
  (let ((max-date (zksummary-db-max-time "daily"))
        (min-date (zksummary-db-min-time "daily")))
    (pcase date
      ((and (pred (string= max-date))
            (guard (> n 0)))
       (message "The latest daily summary!"))
      ((and (pred (string= min-date))
            (guard (< n 0)))
       (message "The earlist daily summary!"))
      (_ (and (setq date (zksummary-inc-date date n))
              (while (= (zksummary-db-count-by-time date) 0)
                (setq date (zksummary-inc-date date n))))))
    date))

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

(defun zksummary-curr-week (&optional date)
  "A list of all date in a week. DATE is a day of the week."
  (let* ((day-seconds (zksummary-date-to-seconds date))
         (calendar-date (zksummary-calendar-date day-seconds))
         (nth (calendar-day-of-week calendar-date))
         (minused-days (let ((days (1- nth)))
                         (if (< days 0) (+ days 7) days)))
         (minused-seconds (* minused-days (* 24 60 60)))
         (first-weekday-seconds (- day-seconds minused-seconds)))
    (zksummary--weekdays-list first-weekday-seconds)))

(defun zksummary-floor-time (time)
  (let* ((timelst (zksummary-ewoc-buffer-data :time))
         (sorted-timelst (sort timelst #'string>)))
    (seq-find (lambda (el)
                (string< el time))
              sorted-timelst)))

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

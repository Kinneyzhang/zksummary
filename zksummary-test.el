(progn
  (zksummary-db-clear)
  (zksummary-db-add "daily" "this is a test note of zksummary, type is daily and time is 2023-02-04" "2023-02-04")
  (zksummary-db-add "daily" "this is a test note of zksummary, type is daily and time is 2023-02-03" "2023-02-03")
  (zksummary-db-add "daily" "this is a test note of zksummary, type is daily and time is 2023-02-02" "2023-02-02")
  (zksummary-db-add "daily" "this is a test note of zksummary, type is daily and time is 2023-02-01" "2023-02-01")
  (zksummary-db-add "daily" "this is a test note of zksummary, type is daily and time is 2023-02-10" "2023-02-10")
  (zksummary-db-add "daily" "this is a test note of zksummary, type is daily and time is 2023-01-14" "2023-01-14")
  (zksummary-db-add "monthly" "2023-01 monthly summary" "2023-01")
  (zksummary-db-add "monthly" "2023-03 monthly summary" "2023-03")
  (zksummary-db-add "monthly" "2022-12 monthly summary" "2022-12")
  (zksummary-db-add "yearly" "2022 summary" "2022")
  (zksummary-db-add "yearly" "2021 summary" "2021")
  (zksummary-db-add "yearly" "2023 summary" "2023"))

(zksummary-db-type-entries "yearly")
(zksummary-db-type-entries "monthly")
(zksummary-db-type-entries "daily")

(zksummary-db-type-entries "weekly")

(zksummary-week-date-lst)

(zksummary-is-week "2023-03-20 2023-03-26")

;; daily -> weekly -> monthly -> yearly

;; zksummary-entry-type: daily,weekly,monthly,yearly
;; zksummary-view-type: daily-date, daily-week....


(defun zksummary-highlight-entry ()
  )

(defun zksummary-entry-prev ()
  )

(defun zksummary-date-at-point ()
  )

(defun zksummary-week-at-point ()
  )

(defun zksummary-month-at-point ()
  )

(defun zksummary-year-at-point ()
  )

(defun zksummary-weekly-show-curr-week ()
  (interactive)
  (zksummary-show "weekly-week"
                  (list (zksummary-week-str
                         (nth 0 zksummary-curr-timeseq)))))

(defun zksummary-monthly-show-curr-month ()
  (interactive)
  (zksummary-show "monthly-month" ))


(defun ewoc-test (ewoc)
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((node (ewoc--node-nth dll -1)))
    (ewoc-data node)))

(defun ewoc-buffer-data ()
  (ewoc-collect zksummary-ewoc 'consp))

(defun ewoc-length ()
  (length (ewoc-buffer-data)))

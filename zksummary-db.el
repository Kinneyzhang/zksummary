;;; zksummary.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: zettelkasten convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/zksummary
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;;; Code:

;;;; Requires

(require 'emacsql)
(require 'emacsql-sqlite)

(defvar zksummary-db-file
  (expand-file-name "zksummary.db" (concat user-emacs-directory "zksummary")))

(defvar zksummary-db--conn (make-hash-table :test #'equal)
  "Database connection to zksummary-db.")

(defconst zksummary-db--table-schemata
  '((summary
     [(id :primary-key)
      (type :not-null)
      (time :not-null)
      (text :not-null)])))

(defun zksummary-db--get-conn ()
  "Return the zksummary database connection with key PATH."
  (gethash zksummary-db-file zksummary-db--conn))

(defun zksummary-db--init (db)
  "Initialize database DB with `zksummary-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) zksummary-db--table-schemata)
      (emacsql db `[:create-table ,table ,schema]))))

(defun zksummary-db ()
  "Entrypoint to zksummary sqlite database."
  (unless (and (zksummary-db--get-conn)
               (emacsql-live-p (zksummary-db--get-conn)))
    (let ((init-db (not (file-exists-p zksummary-db-file))))
      (make-directory (file-name-directory zksummary-db-file) t)
      (let ((conn (emacsql-sqlite zksummary-db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash zksummary-db-file conn zksummary-db--conn)
        (when init-db
          (zksummary-db--init conn)))))
  (zksummary-db--get-conn))

(defun zksummary-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for current zksummary db."
  (unless db
    (setq db (zksummary-db--get-conn)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun zksummary-db-query (sql &rest args)
  "Return SQL query on zksummary database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (zksummary-db) (apply #'format sql args))
    (apply #'emacsql (zksummary-db) sql args)))

(defun zksummary-db-clear ()
  "Clear all data in zksummary database."
  (interactive)
  (when (file-exists-p zksummary-db-file)
    (dolist (table (mapcar #'car zksummary-db--table-schemata))
      (zksummary-db-query `[:delete :from ,table]))))

(defun zksummary-db-drop ()
  "Drop the whole zksummary database."
  (interactive)
  (zksummary-db--close)
  (delete-file zksummary-db-file))

(defun zksummary-db-add (type time text)
  (zksummary-db-query `[:insert-into summary :values [,(org-id-uuid) ,type ,time ,text]]))

(defun zksummary-db-type-notes (type)
  (zksummary-db-query `[:select [id time text] :from summary
                                :where (= type ,type) :order-by (desc time)]))

;;; Test

(progn
  (zksummary-db-clear)
  (zksummary-db-add "daily" "2023-02-04" "this is a test note of zksummary, type is daily and time is 2023-02-04")
  (zksummary-db-add "daily" "2023-02-03" "this is a test note of zksummary, type is daily and time is 2023-02-03")
  (zksummary-db-add "daily" "2023-02-02" "this is a test note of zksummary, type is daily and time is 2023-02-02")
  (zksummary-db-add "daily" "2023-02-01" "this is a test note of zksummary, type is daily and time is 2023-02-01")
  (zksummary-db-add "daily" "2023-02-10" "this is a test note of zksummary, type is daily and time is 2023-02-10")
  (zksummary-db-add "daily" "2023-01-14" "this is a test note of zksummary, type is daily and time is 2023-01-14")
  (zksummary-db-add "monthly" "2023-01" "2023-01 monthly summary")
  (zksummary-db-add "monthly" "2023-03" "2023-03 monthly summary")
  (zksummary-db-add "monthly" "2022-12" "2022-12 monthly summary")
  (zksummary-db-add "yearly" "2022" "2022 summary")
  (zksummary-db-add "yearly" "2021" "2021 summary")
  (zksummary-db-add "yearly" "2023" "2023 summary"))

(zksummary-db-type-notes "yearly")
(zksummary-db-type-notes "monthly")
(zksummary-db-type-notes "daily")
(zksummary-db-type-notes "weekly")

;;; Test
(provide 'zksummary-db)

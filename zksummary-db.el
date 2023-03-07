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
      (content :not-null)
      (summary_time :not-null)
      (create_time :not-null)])))

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

(defun zksummary-db-add (type content time)
  (zksummary-db-query
   `[:insert-into summary :values [,(org-id-uuid) ,type ,content ,time
                                   ,(time-convert (current-time) 'integer)]]))

(defun zksummary-db-type-notes (type)
  (zksummary-db-query `[:select [id summary_time type content] :from summary
                                :where (= type ,type) :order-by (desc summary_time)]))

(provide 'zksummary-db)

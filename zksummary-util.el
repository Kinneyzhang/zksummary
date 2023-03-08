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

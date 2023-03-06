(require 'zksummary-db)

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

(defvar zksummary-default-type "daily")

(defun zksummary-default-time-by-type (type)
  (pcase type
    ("daily" (format-time-string "%Y-%m-%d" (current-time)))
    ("monthly" (format-time-string "%Y-%m" (current-time)))
    ("yearly" (format-time-string "%Y" (current-time)))))

(defun zksummary-capture (&optional type)
  (setq zksummary-window-configuration (current-window-configuration))
  (pop-to-buffer zksummary-capture-buffer)
  (setq zksummary-capture-type (or type zksummary-default-type))
  (setq zksummary-capture-time (zksummary-default-time-by-type
                                     zksummary-capture-type))
  (insert (format "%s summary: %s\n\n" zksummary-capture-type zksummary-capture-time))
  (zksummary-capture-mode 1))

(defun zksummary-capture-finalize ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (type time content)
      (if (re-search-forward "^\\(.+\\) +summary: +\\([-0-9]+\\)"
                             (line-end-position) t)
          (progn
            (setq type (match-string-no-properties 1))
            (setq time (match-string-no-properties 2)))
        (user-error "Invalid format of summary meta!"))
      (forward-line 2)
      (setq content (buffer-substring-no-properties (point) (point-max)))
      (zksummary-db-add type content time)))
  (set-window-configuration zksummary-window-configuration)
  (kill-buffer zksummary-capture-buffer))

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
    (let ((id (nth 0 data))
          (summary-time (nth 1 data))
          (type (nth 2 data))
          (content (nth 3 data)))
      (insert (propertize summary-time 'face 'zksummary-time-face))
      (insert "\n")
      (insert (propertize content 'id id) "\n\n"))))

;;;###autoload
(defun zksummary-show (&optional type)
  "Show a type of zksummary notes, defaultly to `zksummary-default-type'."
  (interactive)
  (let ((buf (get-buffer-create zksummary-buffer))
        (inhibit-read-only 1))
    (with-current-buffer buf
      (zksummay-buffer-setup)
      (let* ((ewoc (ewoc-create 'zksummary-ewoc-pp nil nil t))
             (type (or type zksummary-default-type))
             (summaries (zksummary-db-type-notes type)))
        (setq-local zksummary-ewoc ewoc)
        (setq-local zksummary-current-type type)
        (dolist (summary summaries)
          (ewoc-enter-last ewoc summary)))
      (read-only-mode 1)
      (switch-to-buffer buf))))

;;;###autoload
(defun zksummary-daily-show ()
  (interactive)
  (zksummary-show "daily"))

;;;###autoload
(defun zksummary-monthly-show ()
  (interactive)
  (zksummary-show "monthly"))

;;;###autoload
(defun zksummary-yearly-show ()
  (interactive)
  (zksummary-show "yearly"))

;;;###autoload
(defun zksummary-add ()
  (interactive)
  (zksummary-capture zksummary-current-type))

;;;###autoload
(defun zksummary-delete ()
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (zksummary-db-delete id)))

;;;###autoload
(defun zksummary-edit ()
  (interactive)
  )

;;;###autoload
(defun zksummary-refresh ()
  (interactive)
  (zksummary-show zksummary-current-type))

(defvar zksummary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'zksummary-add)
    (define-key map "D" #'zksummary-delete)
    (define-key map "g" #'zksummary-refresh)
    map))

(provide 'zksummary)

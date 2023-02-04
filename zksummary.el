(require 'zksummary-db)

(defvar zksummary-buffer "*zksummary*")
(defvar zksummary-default-type "weekly")
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
(defvar-local zksummary-capture-note-type nil)
(defvar-local zksummary-capture-note-time nil)

(defun zksummary-capture (type time)
  (setq zksummary-window-configuration (current-window-configuration))
  (pop-to-buffer zksummary-capture-buffer)
  (setq zksummary-capture-note-type type)
  (setq zksummary-capture-note-time time)
  (insert (format "%s summary: %s\n\n" type time))
  (zksummary-capture-mode 1))

(defun zksummary-capture-finalize ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let ((text (buffer-substring-no-properties (point) (point-max))))
      (zksummary-db-add zksummary-capture-note-type zksummary-capture-note-time text)))
  (set-window-configuration zksummary-window-configuration)
  (kill-buffer zksummary-capture-buffer))

(defun zksummary-capture-kill ()
  (interactive)
  (set-window-configuration zksummary-window-configuration)
  (kill-buffer zksummary-capture-buffer))

(defun zksummary-check-time (type time)
  "Check if TIME is a valid TYPE of time."
  t)

;;;###autoload
(defun zksummary-add ()
  (interactive)
  (let ((type (completing-read "Choose a summary type: " zksummary-type-list nil t))
        (time (completing-read "Input a summary time: " nil)))
    (if (zksummary-check-time type time )
        (zksummary-capture type time)
      (user-error "[%] is an invalid time format of type [%s]" time type))))

;;;###autoload
(defun zksummary-show ()
  "Show a type of zksummary notes, defaultly to `zksummary-default-type'."
  (interactive)
  (let ((buf (get-buffer-create zksummary-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only 1)
            (ewoc (ewoc-create 'zksummary-ewoc-pp))
            (summaries (zksummary-db-type-notes zksummary-default-type)))
        (dolist (summary summaries)
          (ewoc-enter-last)))
      (read-only-mode 1)
      (switch-to-buffer buf))))

(provide 'zksummary)

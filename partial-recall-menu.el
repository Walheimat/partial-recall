;;; partial-recall-menu.el --- Menu for `partial-recall' buffers -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>

;;; Commentary:

;; A buffer menu for `partial-recall' buffers.

;;; Code:

(require 'partial-recall (expand-file-name "./partial-recall.el") t)

(defvar prm--buffer "*Partial Recall Menu*")
(defvar prm--null "-")
(defvar prm--present "*")
(defvar prm--list-format (vector
                          '("Buffer" 30 t)
                          '("Tab" 20 t)
                          '("Timestamp" 9 t :pad-right 2)
                          '("Implanted" 9 t)
                          '("Updates" 4 t)))

(defun prm--revert ()
  "Revert the buffer menu."
  (let ((entries nil))

    (dolist (memory (hash-table-values partial-recall--table))

      (let* ((real (eq memory (partial-recall--reality)))
             (tab-name (partial-recall--tab-name memory))
             (mem-pp (prm--print-memory memory real)))

        (dolist (moment (ring-elements (partial-recall--memory-ring memory)))

          (let* ((buffer (partial-recall--moment-buffer moment))
                 (name (buffer-name buffer))
                 (count (prm--print-update-count (partial-recall--moment-update-count moment)))
                 (ts (prm--print-timestamp (partial-recall--moment-timestamp moment)))
                 (implanted (prm--print-permanence (partial-recall--moment-permanence moment)))
                 (item (list tab-name buffer real))
                 (line (vector name mem-pp ts implanted count)))

            (push (list item line) entries)))))

    (setq tabulated-list-format prm--list-format
          tabulated-list-entries (nreverse entries)))

  (tabulated-list-init-header))

(defun prm--list ()
  "List the buffers."
  (let ((buffer (get-buffer-create prm--buffer)))

    (with-current-buffer buffer
      (prm-mode)
      (prm--revert)
      (tabulated-list-print))
    buffer))

(defun prm--entry ()
  "Get current entry as a list of its properties."
  (let ((entry (tabulated-list-get-id)))

    (if (null entry)
        (error "No entry on this line")
      entry)))

(defmacro prm--with-props (args &rest body)
  "Execute BODY with properties bound to ARGS."
  (declare (indent defun))
  `(cl-destructuring-bind ,args (prm--entry)
     ,@body))

(defun prm--switch (fun &optional no-other-tab)
  "Switch using FUN.

If NO-OTHER-TAB is t, raise an error if that would be necessary."
  (prm--with-props (tab buffer real)
    (unless real
      (if no-other-tab
          (user-error "Can't switch tabs")
        (tab-bar-switch-to-tab tab)))
    (funcall fun buffer)))

;; Utility

(defun prm--print-timestamp (timestamp)
  "Format TIMESTAMP."
  (format-time-string "%H:%M:%S" (seconds-to-time timestamp)))

(defun prm--print-update-count (update-count)
  "Format UPDATE-COUNT."
  (if (zerop update-count)
      prm--null
    (number-to-string update-count)))

(defun prm--print-permanence (permanence)
  "Format PERMANENCE."
  (if permanence prm--present prm--null))

(defun prm--print-memory (memory real)
  "Format MEMORY depending on whether it is REAL."
  (let ((orig-size (partial-recall--memory-orig-size memory))
        (actual-size (ring-size (partial-recall--memory-ring memory)))
        (tab-name (if real prm--null (partial-recall--tab-name memory))))

    (if (not (eq orig-size actual-size))
        (format "%s (+%d)" tab-name (- actual-size orig-size))
      tab-name)))

;; Mode

(defvar-keymap prm-mode-map
  :doc "Local keymap for `partial-recall-menu-mode' buffers."
  "RET" #'prm-switch-to-buffer
  "e" #'prm-switch-to-buffer
  "o" #'prm-switch-to-buffer-other-window

  "c" #'prm-reclaim-buffer
  "r" #'prm-reinforce-buffer
  "f" #'prm-forget-buffer
  "i" #'prm-implant-buffer)

(define-derived-mode prm-mode tabulated-list-mode "Partial Recall Menu"
  :interactive nil
  (add-hook 'tabulated-list-revert-hook 'prm--revert nil t))

;; API

(defun prm-switch-to-buffer ()
  "Switch to buffer.

If OTHER-WINDOW is t, do that."
  (interactive nil partial-recall-menu-mode)

  (prm--switch #'switch-to-buffer))

(defun prm-switch-to-buffer-other-window ()
  "Switch to buffer.

If OTHER-WINDOW is t, do that."
  (interactive nil partial-recall-menu-mode)

  (prm--switch #'switch-to-buffer-other-window t))

(defun prm-reclaim-buffer ()
  "Reclaim buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab buffer real)
    (if real
        (user-error "Buffer is part of reality")
      (partial-recall--reclaim buffer t))))

(defun prm-reinforce-buffer ()
  "Reinforce buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab buffer real)
    (if (not real)
        (user-error "Buffer is not part of reality")
      (partial-recall--reinforce buffer t))))

(defun prm-forget-buffer ()
  "Forget the buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab buffer _real)
    (partial-recall--forget buffer)))

(defun prm-implant-buffer (&optional excise)
  "Implant the buffer.

If EXCISE is t, do that instead."
  (interactive "P" partial-recall-menu-mode)

  (prm--with-props (_tab buffer _real)
    (partial-recall--implant buffer excise)))

;;;###autoload
(defun partial-recall-menu ()
  "Display a buffer menu for `partial-recall'."
  (interactive)

  (display-buffer (prm--list)))

(provide 'partial-recall-menu)

;;; partial-recall-menu.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("prm-" . "partial-recall-menu-"))
;; End:

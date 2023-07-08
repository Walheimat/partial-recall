;;; partial-recall-menu.el --- Menu for `partial-recall' buffers -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A buffer menu for `partial-recall' buffers.

;;; Code:

(require 'ring)
(require 'subr-x)
(require 'partial-recall)

(defvar prm--buffer "*Partial Recall Menu*")
(defvar prm--empty " ")
(defvar prm--null "-")
(defvar prm--present "*")
(defvar prm--persistence ["░" "▒" "▓"])
(defvar prm--excess-time (* 60 60 12))

(defvar-local prm--subconscious nil)

(defun prm--format (buffers tabs)
  "Get format using BUFFERS and TABS."
  (let* ((buffers-sorted (sort (mapcar #'length buffers) #'>))
         (longest-buffer (car-safe buffers-sorted))
         (tabs-sorted (sort (mapcar #'length tabs) #'>))
         (longest-tab (car-safe tabs-sorted)))

    (vector
     '("A" 1 t :pad-right 0)
     '("I" 1 t :pad-right 0)
     '("U" 1 t :pad-right 1)
     `("Buffer" ,(or longest-buffer 6) t)
     `("Tab" ,(or longest-tab 6) t)
     '("Timestamp" 9 t))))

(defun prm--revert (&optional include-subconscious)
  "Revert the buffer menu.

The menu will not include the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (let* ((entries nil)
         (buffer-names nil)
         (tab-names nil)
         (filter (if (or include-subconscious
                         prm--subconscious)
                     #'identity
                   (lambda (it) (not (pr--subconscious-p it)))))
         (memories (seq-filter filter (pr--memories))))

    (dolist (memory memories)

      (let* ((real (eq memory (pr--reality)))
             (sub (pr--subconscious-p memory))
             (tab-name (pr--tab-name memory))
             (mem-pp (prm--print-memory memory)))

        (push tab-name tab-names)

        (dolist (moment (ring-elements (mem-ring memory)))

          (let* ((buffer (mom-buffer moment))
                 (name (buffer-name buffer))
                 (count (prm--print-update-count (mom-update-count moment)))
                 (ts (prm--print-timestamp (mom-timestamp moment)))
                 (implanted (prm--print-permanence (mom-permanence moment)))
                 (item (list tab-name buffer real sub))
                 (line (vector prm--empty implanted count name mem-pp ts)))

            (push name buffer-names)

            (push (list item line) entries)))))

    (when include-subconscious
      (setq-local prm--subconscious t))

    (setq tabulated-list-format (prm--format buffer-names tab-names)
          tabulated-list-entries (nreverse entries)))

  (tabulated-list-init-header))

(defun prm--list (&optional include-subconscious)
  "List the buffers.

Includes subconscious buffers if INCLUDE-SUBCONSCIOUS is t."
  (let ((buffer (get-buffer-create prm--buffer)))

    (with-current-buffer buffer
      (prm-mode)
      (prm--revert include-subconscious)
      (tabulated-list-print))
    buffer))

(defun prm--id ()
  "Get current ID object."
  (let ((id (tabulated-list-get-id)))

    (if (null id)
        (error "Nothing on this line")
      id)))

(defmacro prm--with-props (args &rest body)
  "Execute BODY with properties bound to ARGS."
  (declare (indent defun))
  `(cl-destructuring-bind ,args (prm--id)
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
  (if (< prm--excess-time
         (- (floor (time-to-seconds))
            timestamp))
      (format-time-string "   %d/%m" (seconds-to-time timestamp))
    (format-time-string "%H:%M:%S" (seconds-to-time timestamp))))

(defun prm--print-update-count (update-count)
  "Format UPDATE-COUNT."
  (let ((threshold partial-recall-auto-implant-threshold))

    (cond
     ((>= update-count threshold)
      (aref prm--persistence 2))
     ((> update-count (/ threshold 2))
      (aref prm--persistence 1))
     ((> update-count (/ threshold 3))
      (aref prm--persistence 0))
     (t prm--empty))))

(defun prm--print-permanence (permanence)
  "Format PERMANENCE."
  (if permanence prm--present prm--empty))

(defun prm--print-memory (memory)
  "Format MEMORY."
  (let ((orig-size (mem-orig-size memory))
        (actual-size (ring-size (mem-ring memory)))
        (tab-name (cond
                   ((pr--reality-p memory)
                    prm--present)
                   ((pr--subconscious-p memory)
                    prm--null)
                   (t
                    (pr--tab-name memory)))))

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
  "i" #'prm-implant-buffer
  "x" #'prm-execute)

(define-derived-mode prm-mode tabulated-list-mode "Partial Recall Menu"
  :interactive nil
  (add-hook 'tabulated-list-revert-hook 'prm--revert nil t))

;; API

(defun partial-recall-menu-execute ()
  "Forget, steal and implant buffers."
  (interactive nil partial-recall-menu-mode)

  (save-excursion
    (goto-char (point-min))

    (let ((needs-update nil))

      (while (not (eobp))
        (let ((id (tabulated-list-get-id))
              (entry (tabulated-list-get-entry)))

          (if (null entry)
              (forward-line 1)
            (let ((action (aref entry 0))
                  (buffer (nth 1 id))
                  (sub (nth 3 id)))

              (when (and (not needs-update)
                         (not (equal action prm--empty)))
                (setq needs-update t))

              (cond ((equal action "C")
                     (if sub
                         (pr--lift buffer)
                       (pr--reclaim buffer t))
                     (tabulated-list-set-col 0 prm--empty t))
                    ((equal action "F")
                     (pr--forget buffer t))
                    ((equal action "R")
                     (pr--reinforce buffer)
                     (tabulated-list-set-col 0 prm--empty t))
                    ((equal action "I")
                     (pr--implant buffer)
                     (tabulated-list-set-col 0 prm--empty t))
                    ((equal action "X")
                     (pr--implant buffer t)
                     (tabulated-list-set-col 0 prm--empty t))
                    (t nil))

              (forward-line 1)))))

      (when needs-update
        (tabulated-list-revert)))))

(defun partial-recall-menu-switch-to-buffer ()
  "Switch to buffer.

If OTHER-WINDOW is t, do that."
  (interactive nil partial-recall-menu-mode)

  (prm--switch #'switch-to-buffer))

(defun partial-recall-menu-switch-to-buffer-other-window ()
  "Switch to buffer.

If OTHER-WINDOW is t, do that."
  (interactive nil partial-recall-menu-mode)

  (prm--switch #'switch-to-buffer-other-window t))

(defun partial-recall-menu-reclaim-buffer ()
  "Reclaim buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab _buffer real _sub)
    (if real
        (user-error "Buffer is part of reality")
      (tabulated-list-set-col 0 "C" t)
      (forward-line 1))))

(defun partial-recall-menu-reinforce-buffer ()
  "Reinforce buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab _buffer real sub)
    (cond
     (sub
      (user-error "Can't reinforce subconscious buffer"))
     ((not real)
      (user-error "Buffer is not part of reality"))
     (t
      (tabulated-list-set-col 0 "R" t)
      (forward-line 1)))))

(defun partial-recall-menu-forget-buffer ()
  "Forget the buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab _buffer _real _sub)
    (tabulated-list-set-col 0 "F" t)
    (forward-line 1)))

(defun partial-recall-menu-implant-buffer (&optional excise)
  "Implant the buffer.

If EXCISE is t, do that instead."
  (interactive "P" partial-recall-menu-mode)

  (prm--with-props (_tab _buffer _real sub)
    (if sub
        (user-error "Can't implant subconscious buffer")
      (tabulated-list-set-col 0 (if excise "X" "I") t)
      (forward-line 1))))

;;;###autoload
(defun partial-recall-menu (&optional include-subconscious)
  "Display a buffer menu for `partial-recall'.

If INCLUDE-SUBCONSCIOUS is t, the list will include those
buffers."
  (interactive "P")

  (display-buffer (prm--list include-subconscious)))

(provide 'partial-recall-menu)

;;; partial-recall-menu.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("prm-" . "partial-recall-menu-")
;;                          ("pr-" . "partial-recall-")
;;                          ("mom-" . "partial-recall--moment-")
;;                          ("mem-" . "partial-recall--memory-"))
;; End:

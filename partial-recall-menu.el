;;; partial-recall-menu.el --- Menu for `partial-recall' buffers -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.8.4
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
(defvar prm--persistence-blocks ["▂" "▄" "▆" "█"])
(defvar prm--persistence-indicator "░")
(defvar prm--persistence-ratios '(0.25 0.5 0.75 1))
(defvar prm--excess-time (* 60 60 12))

(defvar-local prm--subconscious nil)

(defun prm--format (buffers tabs)
  "Get format using BUFFERS and TABS."
  (let* ((buffers-sorted (sort (mapcar #'length buffers) #'>))
         (longest-buffer (max (or (car-safe buffers-sorted) 6)))
         (tabs-sorted (sort (mapcar #'length tabs) #'>))
         (longest-tab (max (or (car-safe tabs-sorted) 0) 3)))

    (vector
     '("A" 1 t :pad-right 0)
     '("P" 1 t :pad-right 1)
     `("Buffer" ,longest-buffer t)
     `("Tab" ,longest-tab t)
     '("Timestamp" 9 t))))

(defun prm--revert (&optional include-subconscious)
  "Revert the buffer menu.

The menu will not include the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (let* ((entries nil)
         (buffer-names nil)
         (tab-names nil)
         (memories (pr--memories (not (or include-subconscious
                                          prm--subconscious)))))

    (dolist (memory memories)

      (let* ((real (eq memory (pr--reality)))
             (sub (pr--subconscious-p memory))
             (tab-name (prm--tab-name memory))
             (frame (prm--frame memory))
             (mem-pp (prm--print-memory memory)))

        (unless (eq memory (partial-recall--reality))
          (push tab-name tab-names))

        (dolist (moment (ring-elements (mem-ring memory)))

          (let* ((buffer (mom-buffer moment))
                 (name (buffer-name buffer))
                 (ts (prm--print-timestamp (mom-timestamp moment)))
                 (implanted (mom-permanence moment))
                 (presence (prm--print-presence (mom-focus moment) implanted))
                 (item (list tab-name frame buffer real sub))
                 (line (vector prm--empty presence name mem-pp ts)))

            (push name buffer-names)

            (push (list item line) entries)))))

    (when include-subconscious
      (setq prm--subconscious t))

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

(defun prm--display ()
  "Switch using FUN.

If NO-OTHER-TAB is t, raise an error if that would be necessary."
  (prm--with-props (tab frame buffer real _sub)
    (with-selected-frame frame
      (unless real
        (tab-bar-switch-to-tab tab))

     (display-buffer-use-some-window buffer nil))))

;;; -- Utility

(defun prm--tab (memory)
  "Get tab and frame for MEMORY."
  (if-let* ((tab (partial-recall--tab memory)))

      (list tab (selected-frame) nil)

    (when-let* ((info (pr--from-other-frame memory))
                (tab (plist-get info :tab))
                (frame (plist-get info :frame)))
      (list tab frame t))))

(defun prm--frame (memory)
  "Get frame for MEMORY."
  (when-let ((result (prm--tab memory)))

    (nth 1 result)))

(defun prm--tab-name (memory &optional indicate-foreign-frame)
  "Get tab name for MEMORY.

This will also consider other frames. If INDICATE-FOREIGN-FRAME
is t, the name will be propertized."
  (if-let ((result (prm--tab memory)))

    (cl-destructuring-bind (tab _frame foreign) result

      (if (and foreign indicate-foreign-frame)
          (propertize (alist-get 'name tab) 'face 'shadow)
        (alist-get 'name tab)))
    "?"))

(defun prm--print-timestamp (timestamp)
  "Format TIMESTAMP."
  (if (< prm--excess-time
         (- (floor (time-to-seconds))
            timestamp))
      (format-time-string "   %d/%m" (seconds-to-time timestamp))
    (format-time-string "%H:%M:%S" (seconds-to-time timestamp))))

(defun prm--print-presence (focus implanted)
  "Format presence using FOCUS.

If the moment is IMPLANTED, signal that."
  (let* ((threshold partial-recall-auto-implant-threshold)
         (index 0)
         (max-index (1- (length prm--persistence-ratios)))
         (text (if (zerop focus)
                   (if implanted
                       prm--persistence-indicator
                     prm--empty)
                 (while (and (> focus (* threshold (nth index prm--persistence-ratios)))
                             (< index max-index))
                   (setq index (1+ index)))

                 (aref prm--persistence-blocks index)))
         (face (if implanted 'success 'shadow))
         (help (format "Focus: %s, Implanted: %s" focus implanted)))

    (propertize text 'face face 'help-echo help)))

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
                    (prm--tab-name memory t)))))

    (if (not (eq orig-size actual-size))
        (format "%s (+%d)" tab-name (- actual-size orig-size))
      tab-name)))

;; Mode

(defvar-keymap prm-mode-map
  :doc "Local keymap for `partial-recall-menu-mode' buffers."
  "RET" #'prm-display-buffer
  "e" #'prm-display-buffer
  "s" #'prm-toggle-subconscious

  "c" #'prm-reclaim-buffer
  "r" #'prm-reinforce-buffer
  "f" #'prm-forget-buffer
  "i" #'prm-implant-buffer
  "x" #'prm-execute
  "u" #'prm-unmark)

(define-derived-mode prm-mode tabulated-list-mode "Partial Recall Menu"
  :interactive nil
  (add-hook 'tabulated-list-revert-hook 'prm--revert nil t))

;; API

(defun partial-recall-menu-toggle-subconscious ()
  "Toggle the inclusion of the subconscious."
  (interactive)

  (setq prm--subconscious (not prm--subconscious))
  (tabulated-list-revert))

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
                  (buffer (nth 2 id))
                  (sub (nth 4 id)))

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

(defun partial-recall-menu-display-buffer ()
  "Switch to buffer.

If OTHER-WINDOW is t, do that."
  (interactive nil partial-recall-menu-mode)

  (prm--display))

(defun partial-recall-menu-reclaim-buffer ()
  "Reclaim buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab _frame _buffer real _sub)
    (if real
        (user-error "Buffer is part of reality")
      (tabulated-list-set-col 0 "C" t)
      (forward-line 1))))

(defun partial-recall-menu-reinforce-buffer ()
  "Reinforce buffer."
  (interactive nil partial-recall-menu-mode)

  (prm--with-props (_tab _frame _buffer real sub)
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

  (prm--with-props (_tab _frame _buffer _real _sub)
    (tabulated-list-set-col 0 "F" t)
    (forward-line 1)))

(defun partial-recall-menu-implant-buffer (&optional excise)
  "Implant the buffer.

If EXCISE is t, do that instead."
  (interactive "P" partial-recall-menu-mode)

  (prm--with-props (_tab _frame _buffer _real sub)
    (if sub
        (user-error "Can't implant subconscious buffer")
      (tabulated-list-set-col 0 (if excise "X" "I") t)
      (forward-line 1))))

(defun partial-recall-menu-unmark ()
  "Unmark the current marking."
  (interactive)

  (and-let* ((entry (tabulated-list-get-entry))
             (mark (aref entry 0))
             ((not (string= mark " "))))

    (tabulated-list-set-col 0 " " t)
    (forward-line 1)))

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

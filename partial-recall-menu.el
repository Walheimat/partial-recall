;;; partial-recall-menu.el --- Menu for `partial-recall' buffers -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.8.4
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A buffer menu for `partial-recall' buffers.

;;; Code:

(require 'ring)
(require 'subr-x)
(require 'partial-recall)

(defvar partial-recall-menu--buffer "*Partial Recall Menu*")
(defvar partial-recall-menu--empty " ")
(defvar partial-recall-menu--null "-")
(defvar partial-recall-menu--present "*")
(defvar partial-recall-menu--missing "?")
(defvar partial-recall-menu--persistence-blocks ["▂" "▄" "▆" "█"])
(defvar partial-recall-menu--persistence-indicator "░")
(defvar partial-recall-menu--persistence-ratios '(0.25 0.5 0.75 1))
(defvar partial-recall-menu--excess-time (* 60 60 12))

(defvar-local partial-recall-menu--subconscious nil)

(defun partial-recall-menu--format (buffers tabs)
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

(defun partial-recall-menu--revert (&optional include-subconscious)
  "Revert the buffer menu.

The menu will not include the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (let* ((entries nil)
         (buffer-names nil)
         (tab-names nil)
         (memories (partial-recall--memories (not (or include-subconscious
                                          partial-recall-menu--subconscious)))))

    (dolist (memory memories)

      (let* ((real (eq memory (partial-recall--reality)))
             (sub (partial-recall--subconscious-p memory))
             (tab-name (partial-recall-menu--tab-name memory))
             (frame (partial-recall-menu--frame memory))
             (partial-recall--memory-pp (partial-recall-menu--print-memory memory)))

        (unless (eq memory (partial-recall--reality))
          (push tab-name tab-names))

        (dolist (moment (ring-elements (partial-recall--memory-ring memory)))

          (let* ((buffer (partial-recall--moment-buffer moment))
                 (implanted (partial-recall--moment-permanence moment))

                 (pp-buffer-name (partial-recall-menu--print-buffer buffer))
                 (pp-ts (partial-recall-menu--print-timestamp (partial-recall--moment-timestamp moment)))
                 (pp-presence (partial-recall-menu--print-presence (partial-recall--moment-focus moment) implanted))

                 (item (list tab-name frame buffer real sub))
                 (line (vector partial-recall-menu--empty pp-presence pp-buffer-name partial-recall--memory-pp pp-ts)))

            (push pp-buffer-name buffer-names)

            (push (list item line) entries)))))

    (when include-subconscious
      (setq partial-recall-menu--subconscious t))

    (setq tabulated-list-format (partial-recall-menu--format buffer-names tab-names)
          tabulated-list-entries (nreverse entries)))

  (tabulated-list-init-header))

(defun partial-recall-menu--list (&optional include-subconscious)
  "List the buffers.

Includes subconscious buffers if INCLUDE-SUBCONSCIOUS is t."
  (let ((buffer (get-buffer-create partial-recall-menu--buffer)))

    (with-current-buffer buffer
      (partial-recall-menu-mode)
      (partial-recall-menu--revert include-subconscious)
      (tabulated-list-print))
    buffer))

(defun partial-recall-menu--id ()
  "Get current ID object."
  (let ((id (tabulated-list-get-id)))

    (if (null id)
        (error "Nothing on this line")
      id)))

(defmacro partial-recall-menu--with-props (args &rest body)
  "Execute BODY with properties bound to ARGS."
  (declare (indent defun))
  `(cl-destructuring-bind ,args (partial-recall-menu--id)
     ,@body))

(defun partial-recall-menu--display ()
  "Switch using FUN.

If NO-OTHER-TAB is t, raise an error if that would be necessary."
  (partial-recall-menu--with-props (tab frame buffer real _sub)
    (with-selected-frame frame
      (unless real
        (tab-bar-switch-to-tab tab))

     (display-buffer-use-some-window buffer nil))))

;;; -- Utility

(defun partial-recall-menu--tab-and-frame (memory)
  "Get tab and frame for MEMORY."
  (if-let* ((tab (partial-recall--tab memory)))

      (list tab (selected-frame) nil)

    (when-let* ((info (partial-recall--from-other-frame memory))
                (tab (plist-get info :tab))
                (frame (plist-get info :frame)))
      (list tab frame t))))

(defun partial-recall-menu--frame (memory)
  "Get frame for MEMORY."
  (when-let ((result (partial-recall-menu--tab-and-frame memory)))

    (nth 1 result)))

(defun partial-recall-menu--tab-name (memory &optional indicate-foreign-frame)
  "Get tab name for MEMORY.

This will also consider other frames. If INDICATE-FOREIGN-FRAME
is t, the name will be propertized."
  (or (if-let ((result (partial-recall-menu--tab-and-frame memory)))

          (cl-destructuring-bind (tab _frame foreign) result

            (if (and foreign indicate-foreign-frame)
                (propertize (alist-get 'name tab) 'face 'shadow)
              (alist-get 'name tab)))
        partial-recall-menu--missing)))

;;; -- Printing

(defun partial-recall-menu--print-buffer (buffer)
  "Print BUFFER."
  (or (buffer-name buffer)
      partial-recall-menu--missing))

(defun partial-recall-menu--print-timestamp (timestamp)
  "Format TIMESTAMP."
  (if (< partial-recall-menu--excess-time
         (- (floor (time-to-seconds))
            timestamp))
      (format-time-string "   %d/%m" (seconds-to-time timestamp))
    (format-time-string "%H:%M:%S" (seconds-to-time timestamp))))

(defun partial-recall-menu--print-presence (focus implanted)
  "Format presence using FOCUS.

If the moment is IMPLANTED, signal that."
  (let* ((threshold partial-recall-auto-implant-threshold)
         (index 0)
         (max-index (1- (length partial-recall-menu--persistence-ratios)))
         (text (if (zerop focus)
                   (if implanted
                       partial-recall-menu--persistence-indicator
                     partial-recall-menu--empty)
                 (while (and (> focus (* threshold (nth index partial-recall-menu--persistence-ratios)))
                             (< index max-index))
                   (setq index (1+ index)))

                 (aref partial-recall-menu--persistence-blocks index)))
         (face (if implanted 'success 'shadow))
         (help (format "Focus: %s, Implanted: %s" focus implanted)))

    (propertize text 'face face 'help-echo help)))

(defun partial-recall-menu--print-memory (memory)
  "Format MEMORY."
  (let ((orig-size (partial-recall--memory-orig-size memory))
        (actual-size (ring-size (partial-recall--memory-ring memory)))
        (tab-name (cond
                   ((partial-recall--reality-p memory)
                    partial-recall-menu--present)
                   ((partial-recall--subconscious-p memory)
                    partial-recall-menu--null)
                   (t
                    (partial-recall-menu--tab-name memory t)))))

    (if (not (eq orig-size actual-size))
        (format "%s (+%d)" tab-name (- actual-size orig-size))
      tab-name)))

;;; -- Mode

(defvar-keymap partial-recall-menu-mode-map
  :doc "Local keymap for `partial-recall-menu-mode' buffers."
  "RET" #'partial-recall-menu-display-buffer
  "e" #'partial-recall-menu-display-buffer
  "s" #'partial-recall-menu-toggle-subconscious

  "c" #'partial-recall-menu-reclaim-buffer
  "r" #'partial-recall-menu-reinforce-buffer
  "f" #'partial-recall-menu-forget-buffer
  "i" #'partial-recall-menu-implant-buffer
  "x" #'partial-recall-menu-execute
  "u" #'partial-recall-menu-unmark)

(define-derived-mode partial-recall-menu-mode tabulated-list-mode "Partial Recall Menu"
  :interactive nil
  (add-hook 'tabulated-list-revert-hook 'partial-recall-menu--revert nil t))

;;; -- API

(defun partial-recall-menu-toggle-subconscious ()
  "Toggle the inclusion of the subconscious."
  (interactive)

  (setq partial-recall-menu--subconscious (not partial-recall-menu--subconscious))
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
                         (not (equal action partial-recall-menu--empty)))
                (setq needs-update t))

              (cond ((equal action "C")
                     (if sub
                         (partial-recall--lift buffer)
                       (partial-recall--reclaim buffer t))
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))
                    ((equal action "F")
                     (partial-recall--forget buffer t))
                    ((equal action "R")
                     (partial-recall--reinforce buffer)
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))
                    ((equal action "I")
                     (partial-recall--implant buffer)
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))
                    ((equal action "X")
                     (partial-recall--implant buffer t)
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))
                    (t nil))

              (forward-line 1)))))

      (when needs-update
        (tabulated-list-revert)))))

(defun partial-recall-menu-display-buffer ()
  "Switch to buffer.

If OTHER-WINDOW is t, do that."
  (interactive nil partial-recall-menu-mode)

  (partial-recall-menu--display))

(defun partial-recall-menu-reclaim-buffer ()
  "Reclaim buffer."
  (interactive nil partial-recall-menu-mode)

  (partial-recall-menu--with-props (_tab _frame _buffer real _sub)
    (if real
        (user-error "Buffer is part of reality")
      (tabulated-list-set-col 0 "C" t)
      (forward-line 1))))

(defun partial-recall-menu-reinforce-buffer ()
  "Reinforce buffer."
  (interactive nil partial-recall-menu-mode)

  (partial-recall-menu--with-props (_tab _frame _buffer real sub)
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

  (partial-recall-menu--with-props (_tab _frame _buffer _real _sub)
    (tabulated-list-set-col 0 "F" t)
    (forward-line 1)))

(defun partial-recall-menu-implant-buffer (&optional excise)
  "Implant the buffer.

If EXCISE is t, do that instead."
  (interactive "P" partial-recall-menu-mode)

  (partial-recall-menu--with-props (_tab _frame _buffer _real sub)
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

  (display-buffer (partial-recall-menu--list include-subconscious)))

(provide 'partial-recall-menu)

;;; partial-recall-menu.el ends here

;;; partial-recall-menu.el --- Menu for `partial-recall' buffers -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.13.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A buffer menu for `partial-recall' moments.

;;; Code:

(require 'ring)
(require 'subr-x)
(require 'partial-recall)

;;;; Customization:

(defcustom partial-recall-menu-display-command 'pop-to-buffer
  "The command used to display the buffer."
  :type 'function
  :group 'partial-recall)

(defvar partial-recall-menu--buffer "*Partial Recall Menu*"
  "Name of the menu buffer.")
(defvar partial-recall-menu--empty " "
  "String representation of empty content.")
(defvar partial-recall-menu--null "-"
  "String representation of null content.")
(defvar partial-recall-menu--indicate "*"
  "String used to indicate the current memory.")
(defvar partial-recall-menu--missing "?"
  "String used to indicate missing content.")
(defvar partial-recall-menu--persistence-indicator "░"
  "String used to indicate persistence without focus.")

(defvar partial-recall-menu--excess-time (* 60 60 12)
  "Time in seconds after which time should be displayed differently.")

(defvar-local partial-recall-menu--subconscious nil
  "If t, subconscious buffers will be included in the menu.")

(defun partial-recall-menu--format (buffers tabs)
  "Get format using BUFFERS and TABS."
  (let* ((buffers-sorted (sort (mapcar #'length buffers) #'>))
         (longest-buffer (max (or (car-safe buffers-sorted) 6)))
         (tabs-sorted (sort (mapcar #'length tabs) #'>))
         (longest-tab (max (or (car-safe tabs-sorted) 0) 3)))

    (vector
     '("A" 1 t :pad-right 0)
     '("P" 1 t :pad-right 1)
     '(" " 1 t)
     `("Buffer" ,longest-buffer t)
     '(" " 1 t)
     `("Tab" ,longest-tab t)
     '("Timestamp" 9 t))))


(defun partial-recall-menu--revert (&optional include-subconscious)
  "Revert the buffer menu.

The menu will not include the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (let* ((entries nil)
         (buffer-names nil)
         (tab-names nil)
         (memories (partial-recall-memories)))

    (dolist (memory memories)

      (let* ((real (eq memory (partial-recall--reality)))
             (supp (partial-recall--suppressed (partial-recall-memory--key memory)))
             (tab-name (partial-recall-menu--tab-name memory))
             (frame (partial-recall-menu--frame memory))
             (partial-recall-memory--pp (partial-recall-menu--print-memory memory))
             (at-capacity (partial-recall-memory--near-capacity-p memory))
             (moments (ring-elements (partial-recall-memory--moments memory))))

        (push tab-name tab-names)

        (dotimes (i (length moments))

          (let* ((moment (nth i moments))
                 (buffer (partial-recall-moment--buffer moment))
                 (implanted (partial-recall-moment--permanence moment))
                 (moment-state (cond
                                ((and at-capacity
                                      (eq i (1- (length moments))))
                                 'at-risk)
                                ((partial-recall--intermediate-term-p moment)
                                 'intermediate)
                                (t nil)))

                 (pp-buffer-name (partial-recall-menu--print-buffer buffer moment-state))
                 (pp-modified (partial-recall-menu--print-buffer-status buffer))
                 (pp-ts (partial-recall-menu--print-timestamp (partial-recall-moment--timestamp moment)))
                 (pp-presence (partial-recall-menu--print-presence (partial-recall-moment--focus moment) implanted))
                 (pp-real (if real partial-recall-menu--indicate partial-recall-menu--empty))


                 (item (list tab-name frame buffer real nil))
                 (line (vector partial-recall-menu--empty
                               pp-presence
                               pp-modified
                               pp-buffer-name
                               pp-real
                               partial-recall-memory--pp
                               pp-ts)))

            (push pp-buffer-name buffer-names)

            (push (list item line) entries)))

        (when (or include-subconscious
                  partial-recall-menu--subconscious)
          (dotimes (i (length supp))

            (let* ((buffer (nth i supp))
                   (moment-state 'subconscious)

                   (pp-buffer-name (partial-recall-menu--print-buffer buffer moment-state))
                   (pp-modified (partial-recall-menu--print-buffer-status buffer))
                   (pp-ts "")
                   (pp-presence (partial-recall-menu--print-presence 0 nil))
                   (pp-real partial-recall-menu--empty)

                   (item (list tab-name frame buffer nil t))
                   (line (vector partial-recall-menu--empty
                                 pp-presence
                                 pp-modified
                                 pp-buffer-name
                                 pp-real
                                 (propertize partial-recall-memory--pp
                                             'face 'shadow)
                                 pp-ts)))

              (push pp-buffer-name buffer-names)

              (push (list item line) entries)))

          (setq partial-recall-menu--subconscious t))))

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
  "Display the entry.

Raises and error if the entry belongs to the subconscious."
  (partial-recall-menu--with-props (tab frame buffer real sub)

    (when sub
      (user-error "Cannot display subconscious moment"))

    (with-selected-frame frame
      (unless real
        (tab-bar-switch-to-tab tab))

      (display-buffer-use-some-window buffer nil))))

;;;; Utility

(defun partial-recall-menu--tab-and-frame (memory)
  "Get tab and frame for MEMORY."
  (if-let* ((tab (partial-recall--find-tab-from-memory memory)))

      (list tab (selected-frame) nil)

    (when-let* ((info (partial-recall-memory--find-in-other-frame memory))
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

;;;; Printing

(defun partial-recall-menu--print-buffer (buffer &optional moment-state)
  "Print BUFFER name.

The name is propertized based on MOMENT-STATE."
  (let* ((name (or (buffer-name buffer) partial-recall-menu--missing))

         (face (pcase moment-state
                 ('at-risk 'partial-recall-alert)
                 ('subconscious 'partial-recall-deemphasized)
                 ('intermediate 'partial-recall-emphasis)
                 (_ nil)))
         (echo (pcase moment-state
                 ('at-risk (format "%s is at risk of being suppressed" name))
                 ('intermediate (format "%s is intermediate" name))
                 (_ name))))

    (propertize name
                'face face
                'help-echo echo)))

(defun partial-recall-menu--print-buffer-status (buffer)
  "Print status of BUFFER."
  (if (buffer-modified-p buffer) partial-recall-menu--indicate partial-recall-menu--empty))

(defun partial-recall-menu--print-timestamp (timestamp)
  "Format TIMESTAMP."
  (if (< partial-recall-menu--excess-time
         (- (floor (time-to-seconds))
            timestamp))
      (propertize (format-time-string "%d/%m/%y" (seconds-to-time timestamp))
                  'face 'partial-recall-deemphasized)
    (format-time-string "%H:%M:%S" (seconds-to-time timestamp))))

(defun partial-recall-menu--print-presence (_focus _implanted)
  "Format presence using FOCUS.

If the moment is IMPLANTED, signal that."
  (propertize partial-recall-menu--empty 'face 'partial-recall-deemphasized))

(defun partial-recall-menu--print-memory (memory)
  "Format MEMORY."
  (let ((orig-size (partial-recall-memory--orig-size memory))
        (actual-size (ring-size (partial-recall-memory--moments memory)))
        (tab-name (cond
                   ((partial-recall--realityp memory)
                    (partial-recall-menu--tab-name memory t))
                   (t
                    (partial-recall-menu--tab-name memory t)))))

    (if (not (eq orig-size actual-size))
        (format "%s (+%d)" tab-name (- actual-size orig-size))
      tab-name)))

;;;; Mode

(defvar-keymap partial-recall-menu-mode-map
  :doc "Local keymap for `partial-recall-menu-mode' buffers."
  "RET" #'partial-recall-menu-display-buffer
  "e" #'partial-recall-menu-display-buffer
  "s" #'partial-recall-menu-toggle-subconscious

  "c" #'partial-recall-menu-reclaim
  "r" #'partial-recall-menu-reinforce
  "f" #'partial-recall-menu-forget
  "i" #'partial-recall-menu-implant
  "x" #'partial-recall-menu-execute
  "m" #'partial-recall-menu-mark
  "u" #'partial-recall-menu-unmark)

(define-derived-mode partial-recall-menu-mode tabulated-list-mode "Partial Recall Menu"
  :interactive nil
  (add-hook 'tabulated-list-revert-hook 'partial-recall-menu--revert nil t))

;;;; API

(defun partial-recall-menu-toggle-subconscious ()
  "Toggle the inclusion of the subconscious."
  (interactive)

  (setq partial-recall-menu--subconscious (not partial-recall-menu--subconscious))
  (tabulated-list-revert))

(defun partial-recall-menu-execute ()
  "Execute actions of marked moments."
  (interactive nil partial-recall-menu-mode)

  (let ((needs-update nil)
        (marked nil))

    (save-excursion
      (goto-char (point-min))

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

              (cond (;; Reclaim or lift.
                     (equal action "C")
                     (if sub
                         (partial-recall--remember buffer)
                       (partial-recall--reclaim buffer t))
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))

                    ;; Forget.
                    ((equal action "F")
                     (partial-recall--forget buffer))

                    ;; Reinforce.
                    ((equal action "R")
                     (partial-recall--reinforce buffer)
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))

                    ;; Implant.
                    ((equal action "I")
                     (partial-recall--set-permanence buffer)
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))

                    ;; Excise.
                    ((equal action "X")
                     (partial-recall--set-permanence buffer t)
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))

                    ((equal action "M")
                     (push buffer marked)
                     (tabulated-list-set-col 0 partial-recall-menu--empty t))
                    (t nil))

              (forward-line 1))))))

    (when needs-update
      (tabulated-list-revert))

    (when marked
      (partial-recall--spin-out marked))))

(defun partial-recall-menu-display-buffer ()
  "Switch to buffer.

If OTHER-WINDOW is t, do that."
  (interactive nil partial-recall-menu-mode)

  (partial-recall-menu--display))

(defun partial-recall-menu-reclaim ()
  "Reclaim buffer."
  (interactive nil partial-recall-menu-mode)

  (partial-recall-menu--with-props (_tab _frame _buffer real _sub)
    (if real
        (user-error "Buffer is part of reality")
      (tabulated-list-set-col 0 "C" t)
      (forward-line 1))))

(defun partial-recall-menu-reinforce ()
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

(defun partial-recall-menu-forget ()
  "Forget the buffer."
  (interactive nil partial-recall-menu-mode)

  (partial-recall-menu--with-props (_tab _frame _buffer _real _sub)
    (tabulated-list-set-col 0 "F" t)
    (forward-line 1)))

(defun partial-recall-menu-implant (&optional excise)
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

(defun partial-recall-menu-mark ()
  "Mark the current entry."
  (interactive)

  (tabulated-list-set-col 0 "M" t)
  (forward-line 1))

;;;###autoload
(defun partial-recall-menu (&optional include-subconscious)
  "Display a buffer menu for `partial-recall'.

If INCLUDE-SUBCONSCIOUS is t, the list will include those
buffers."
  (interactive "P")

  (funcall partial-recall-menu-display-command (partial-recall-menu--list include-subconscious)))

(provide 'partial-recall-menu)

;;; partial-recall-menu.el ends here

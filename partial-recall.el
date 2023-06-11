;;; partial-recall.el --- STM for `tab-bar-mode' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Short-term (buffer) memory for `tab-bar-mode' tabs.
;;
;;`partial-recall' will keep track of a limited number of file buffers
;; that in a ring. Buffers are time-stamped to (1) allow for the
;; memory to grow if the oldest one is still relatively recent and (2)
;; to allow reclaiming buffers from other tabs if they're relatively
;; old.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)

(defgroup partial-recall nil
  "Partially recallable tab buffers."
  :group 'partial-recall)

(defcustom partial-recall-limit 20
  "The amount of buffers to recall.

This limit of a memory may increase if buffers are remembered in
quick succession. See `partial-recall-threshold'."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-threshold (* 5 60)
  "Threshold in seconds that will allow a memory to grow.

If the oldest moment is younger than the threshold, the limit is
increased and the buffer will remain."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-reclaim-threshold (* 10 60)
  "Threshold in seconds that when exceeded allows reclaiming.

Has no effect if `partial-recall-reclaim' is nil."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-reclaim t
  "Whether to automatically reclaim buffers from other memories."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-mode-lighter " pr"
  "The mode line lighter."
  :type 'string
  :group 'partial-recall)

;; Hash table

(defvar partial-recall--table (make-hash-table))

(defun partial-recall--key (&optional tab)
  "Get the hash key of TAB."
  (when-let* ((tab (or tab (tab-bar--current-tab)))
              (key (alist-get 'pr tab)))
    key))

(defun partial-recall--create-hash-key (tab)
  "Create the key for TAB.

This uses a message digest of the tab, a random number, the Emacs
PID and `recent-keys' vector."
  (let ((object (format "%s%s%s%s" tab (random) (emacs-pid) (recent-keys))))

    (md5 object)))

;; Structures

(cl-defstruct (partial-recall--moment
               (:constructor partial-recall--moment-create
                             (buffer &aux (timestamp (floor (time-to-seconds))))))
  "A moment of partial recall.

A moment is defined by a buffer and a timestamp when the buffer
was remembered."
  buffer timestamp)

(cl-defstruct (partial-recall--memory
               (:constructor partial-recall--memory-create
                             (&aux
                              (ring (make-ring partial-recall-limit))
                              (orig-size partial-recall-limit))))
  "A memory of partial recall.

A memory is a ring of moments and the size it had upon
construction."
  ring orig-size)

(defun partial-recall--ring-member (ring buffer)
  "Check if BUFFER is a member of RING."
  (catch 'found
    (dotimes (ind (ring-length ring))
      (when (equal buffer (partial-recall--moment-buffer (ring-ref ring ind)))
        (throw 'found ind)))))

(defun partial-recall--at-capacity (memory)
  "Check if MEMORY is at capacity."
  (let ((ring (partial-recall--memory-ring memory)))
    (eq (ring-length ring) (ring-size ring))))

(defun partial-recall--moments ()
  "Get the moments from the current memory."
  (when-let* ((tab-key (partial-recall--key))

              (table partial-recall--table)
              (memory (gethash tab-key table)))

    (partial-recall--memory-ring memory)))

(defun partial-recall--current-p (buffer)
  "Check if BUFFER belongs to the current tab."
  (when-let ((moments (partial-recall--moments)))

    (partial-recall--ring-member moments buffer)))

(defun partial-recall--has-buffers-p ()
  "Check if there are buffers associated with the current tab."
  (when-let ((moments (partial-recall--moments)))

    (not (ring-empty-p moments))))

(defun partial-recall--known-buffer-p (buffer)
  "Check if BUFFER is recalled at all."
  (let* ((known (cl-loop for _k being the hash-keys of partial-recall--table
                         using (hash-values memory)
                         append (ring-elements (partial-recall--memory-ring memory))))
         (buffers (mapcar #'partial-recall--moment-buffer known)))

    (memq buffer buffers)))

(defun partial-recall--memory-buffer-p (memory buffer)
  "Check if MEMORY does contain BUFFER."
  (partial-recall--ring-member
   (partial-recall--memory-ring memory)
   buffer))

(defun partial-recall--moment-buffer-p (moment buffer)
  "Check if MOMENT does contain BUFFER."
  (eq (partial-recall--moment-buffer moment) buffer))

(defun partial-recall--get-or-create-memory (tab-key)
  "Get or create the memory for TAB-KEY."
  (if-let* ((table partial-recall--table)
            (memory (gethash tab-key table)))
      memory
    (let ((new-memory (partial-recall--memory-create)))
      (puthash tab-key new-memory table)
      new-memory)))

;; Helpers

(defvar partial-recall--last-checked nil)

(defun partial-recall--handle-buffer (buffer)
  "Maybe remember BUFFER."
  (when (buffer-live-p buffer)
    (setq partial-recall--last-checked buffer)

    (if (and partial-recall-reclaim
             (partial-recall--known-buffer-p buffer))
        (partial-recall--reclaim buffer)
      (partial-recall--remember buffer))))

(defun partial-recall--should-extend-p (memory)
  "Check if MEMORY should extend its ring size."
  (when-let* ((ring (partial-recall--memory-ring memory))
              (to-remove (ring-ref ring (1- (ring-length ring)))))

    (> partial-recall-threshold
       (- (floor (time-to-seconds))
          (partial-recall--moment-timestamp to-remove)))))

(defun partial-recall--current ()
  "Get the current memory."
  (gethash (partial-recall--key) partial-recall--table))

(defun partial-recall--owner (&optional buffer)
  "Return the memory that owns BUFFER.

Defaults to the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (memories (hash-table-values partial-recall--table)))

    (seq-find (lambda (it) (partial-recall--memory-buffer-p it buffer)) memories)))

;; Handlers

(defun partial-recall--on-create (tab)
  "Equip TAB with a unique hash key."
  (let ((key (partial-recall--create-hash-key tab))
        (state (cdr tab)))

    (setcdr tab (push (cons 'pr key) state))))

(defun partial-recall--on-close (tab only)
  "Remove TAB from table if it is not the ONLY one."
  (when-let* ((tab-key (partial-recall--key tab))
              (table partial-recall--table))

    (when (and (not only)
               (gethash tab-key table))
      (remhash tab-key table))))

(defvar partial-recall--timer nil)

(defun partial-recall--on-buffer-list-update ()
  "Schedule maybe remembering the current buffer."
  (with-current-buffer (window-buffer)
    (and-let* ((buffer (current-buffer))
               (new (not (eq partial-recall--last-checked buffer)))
               (file-name (buffer-file-name buffer)))

      (when partial-recall--timer
        (cancel-timer partial-recall--timer)
        (setq partial-recall--timer nil))

      (setq partial-recall--timer
            (run-at-time 0.5 nil #'partial-recall--handle-buffer buffer)))))

(defun partial-recall--on-frame-delete (frame)
  "Clear hashes associated with FRAME."
  (let ((tabs (funcall tab-bar-tabs-function frame)))

    (dolist (tab tabs)
      (partial-recall--on-close tab nil))))

(defun partial-recall--remember (&optional buffer)
  "Remember the BUFFER for this tab."
  (when-let* ((tab-key (partial-recall--key))
              (buffer (or buffer (current-buffer)))
              (memory (partial-recall--get-or-create-memory tab-key))
              (ring (partial-recall--memory-ring memory)))

    (unless (partial-recall--ring-member ring buffer)
      (when (and (partial-recall--at-capacity memory)
                 (partial-recall--should-extend-p memory))
        (ring-extend ring 1))

      (ring-insert ring (partial-recall--moment-create buffer)))))

(defun partial-recall--reclaim (&optional buffer force)
  "Reclaim BUFFER if possible.

If FORCE is t, will reclaim even if the threshold wasn't passed."
  (and-let* ((buffer (or buffer (current-buffer)))
             (current-memory (partial-recall--current))
             (owner (partial-recall--owner buffer))
             ((not (eq current-memory owner)))
             (ring (partial-recall--memory-ring owner))
             (moment (seq-find (lambda (it) (partial-recall--moment-buffer-p it buffer)) (ring-elements ring)))
             ((or force
                  (< partial-recall-reclaim-threshold
                     (- (floor (time-to-seconds))
                        (partial-recall--moment-timestamp moment)))))
             (index (partial-recall--ring-member ring buffer)))

    ;; Forget in the old memory.
    (ring-remove ring index)

    ;; Remember in the current one.
    (partial-recall--remember buffer)))

(defun partial-recall--forget (&optional buffer)
  "Forget BUFFER.

If no buffer is passed, the current buffer is used."
  (let* ((buffer (or buffer (current-buffer)))

         (table partial-recall--table)
         (maybe-remove (lambda (_key memory)
                         (when-let* ((ring (partial-recall--memory-ring memory))
                                     (index (partial-recall--ring-member ring buffer)))

                           (ring-remove ring index)))))

    (maphash maybe-remove table)))

;; Integration

(declare-function consult--buffer-state "ext:consult.el")
(declare-function consult--buffer-query "ext:consult.el")

(defvar partial-recall--consult-buffer-source
  (list :name "Partial Recall"
        :narrow ?r
        :category 'buffer
        :state #'consult--buffer-state
        :history 'buffer-name-history
        :items
        #'(lambda () (consult--buffer-query :sort 'visibility
                                       :predicate #'partial-recall--current-p
                                       :as #'buffer-name)))
  "Buffers that are recalled from the current tab.")

;; Setup

(defun partial-recall--fix-up-primary-tab ()
  "Fix up the primary tab."
  (if-let* ((mode tab-bar-mode)
            (tabs (funcall tab-bar-tabs-function))
            (original (nth 0 tabs)))

      (unless (partial-recall--key original)
        (partial-recall--on-create original))
    (message "Might have failed to set up original tab")))

(defun partial-recall--queue-fix-up (&rest _r)
  "Queue a fix-up of the original tab."
  (run-at-time 1.0 nil #'partial-recall--fix-up-primary-tab))

(defun partial-recall-mode--setup ()
  "Set up `partial-recall-mode'."
  (unless tab-bar-mode
    (tab-bar-mode 1))

  (partial-recall--queue-fix-up)

  (add-hook 'after-make-frame-functions #'partial-recall--queue-fix-up)
  (add-hook 'kill-buffer-hook #'partial-recall--forget)
  (add-hook 'buffer-list-update-hook #'partial-recall--on-buffer-list-update)
  (add-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (add-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)
  (add-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

(defun partial-recall-mode--teardown ()
  "Tear down `partial-recall-mode'."
  (remove-hook 'after-make-frame-functions #'partial-recall--queue-fix-up)
  (remove-hook 'kill-buffer-hook #'partial-recall--forget)
  (remove-hook 'buffer-list-update-hook #'partial-recall--on-buffer-list-update)
  (remove-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (remove-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)
  (remove-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

;; API

;;;###autoload
(define-minor-mode partial-recall-mode
  "Keep track of buffers opened in a tab."
  :lighter partial-recall-mode-lighter
  :global t
  (if partial-recall-mode
      (partial-recall-mode--setup)
    (partial-recall-mode--teardown)))

;;;###autoload
(defun partial-recall-remember ()
  "Remember this buffer."
  (interactive)

  (when partial-recall-mode
    (partial-recall--remember (current-buffer))))

;;;###autoload
(defun partial-recall-reclaim (&optional force)
  "Reclaim BUFFER if possible.

If FORCE is t, will reclaim even if the threshold wasn't passed.."
  (interactive "P")

  (when partial-recall-mode
    (partial-recall--reclaim (current-buffer) force)))

;;;###autoload
(defun partial-recall-forget ()
  "Forget current buffer."
  (interactive)

  (when partial-recall-mode
    (partial-recall--forget (current-buffer))))

(provide 'partial-recall)

;;; partial-recall.el ends here

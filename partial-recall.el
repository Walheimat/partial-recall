;;; partial-recall.el --- STM for `tab-bar-mode' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.6.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Short-term (buffer) memory for `tab-bar-mode' tabs.
;;
;;`partial-recall' will keep track of file buffers opened in a tab in
;; a ring. These rings are called memories in the context of this
;; package; the current memory is called reality, all other memories
;; are dreams. Buffers are time-stamped to (1) allow for the memory to
;; grow if the oldest one is still relatively young and (2) to allow
;; reclaiming buffers from other tabs if they're relatively old.
;; Buffers are called moments in the context of this package.
;;
;; Moments can be reclaimed from other memories, they can be
;; forgotten, they can be implanted and they can be reinforced. All of
;; these things happen automatically but can be performed explicitly
;; by the user as well.
;;
;; When moments are forgotten, they are added to a special kind of
;; memory: the subconscious. Once they leave the subconscious as well,
;; their buffers are killed. When a moment is implanted, it will not
;; leave the memory automatically anymore, instead it is reinforced
;; (reinserted). Continually re-visiting a moment will automatically
;; implant it.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)

;;; -- Customization

(defgroup partial-recall nil
  "Short-term (buffer) memory."
  :group 'partial-recall)

(defcustom partial-recall-handle-delay 3
  "The delay in seconds after which a buffer will be handled."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-buffer-limit 10
  "The amount of buffers to recall.

This limit of a memory may increase if buffers are remembered in
quick succession. See `partial-recall-max-age'."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-max-age (* 30 60)
  "Threshold in seconds that will allow a memory to grow.

If the oldest moment is younger than the threshold, the limit is
increased and the buffer will remain."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-reclaim t
  "Whether to automatically reclaim buffers from other memories."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-reclaim-min-age (* 60 60)
  "Threshold in seconds that when exceeded allows reclaiming.

Has no effect if `partial-recall-reclaim' is nil."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-repress t
  "Whether `partial-recall-suppress' may kill buffers.

These are buffers that are removed from the subconscious."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-auto-implant t
  "Whether to automatically implant buffers.

This is will implant buffers that have met
`partial-recall-auto-implant-threshold'."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-auto-implant-threshold 4
  "Minimum of updates before auto-implanting."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-mode-lighter " pr"
  "The mode line lighter."
  :type 'string
  :group 'partial-recall)

(defcustom partial-recall-record-triggers '(consult-buffer)
  "Commands that should trigger recording the buffer."
  :type '(repeat symbol)
  :group 'partial-recall)

(defcustom partial-recall-log nil
  "Whether to log actions."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-log-level 1
  "The degree to which actions are logged."
  :type '(choice (const :tag "Info" 1)
                 (const :tag "Debug" 0)))

;;; -- Internal variables

(defvar partial-recall--table (make-hash-table))
(defvar partial-recall--subconscious-key "subconscious")
(defvar partial-recall--timer nil)
(defvar partial-recall--last-checked nil)
(defvar partial-recall--switch-to-buffer-function #'switch-to-buffer)
(defvar partial-recall--before-minibuffer nil)

;;; -- Structures

(cl-defstruct (partial-recall--moment
               (:constructor partial-recall--moment-create
                             (buffer
                              &aux
                              (timestamp (floor (time-to-seconds)))
                              (update-count 0)
                              (permanence nil))))
  "A moment of partial recall.

A moment is defined by a buffer, a timestamp when that buffer was
first remembered, a count of how many times it was updated and a
permanence marker that can prevent it from being forgotten."
  buffer timestamp update-count permanence)

(cl-defstruct (partial-recall--memory
               (:constructor partial-recall--memory-create
                             (key
                              &aux
                              (ring (make-ring partial-recall-buffer-limit))
                              (orig-size partial-recall-buffer-limit))))
  "A memory of partial recall.

A memory is a key that connects it to the hash table, a ring of
moments and the size it had upon construction."
  key ring orig-size)

;;; -- Accessors

(defun partial-recall--key (&optional tab)
  "Get the hash key of TAB."
  (when-let* ((tab (or tab (tab-bar--current-tab)))
              (key (alist-get 'pr tab)))

    key))

(defun partial-recall--create-key (tab)
  "Create the key for TAB.

This uses a message digest of the tab, a random number, the Emacs
PID and `recent-keys' vector."
  (let ((object (format "%s%s%s%s" tab (random) (emacs-pid) (recent-keys))))

    (md5 object)))

(defun partial-recall--ring-oldest (ring)
  "Get the oldest element in RING."
  (and-let* ((length (ring-length ring))
             ((> length 0)))

    (ring-ref ring (1- length))))

(defun partial-recall--buffer-owner (&optional buffer)
  "Return the memory that owns BUFFER.

Defaults to the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (memories (partial-recall--memories))
         (find (apply-partially #'partial-recall--memory-buffer-p buffer)))

    (seq-find find memories)))

(defun partial-recall--mapped-buffers (&optional include-subconscious)
  "Get all mapped buffers.

This excludes moments in the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (let ((mapped (cl-loop for k being the hash-keys of partial-recall--table
                         using (hash-values memory)
                         unless (and (not include-subconscious)
                                     (string= k partial-recall--subconscious-key))
                         append (ring-elements (partial-recall--memory-ring memory)))))

    (mapcar #'partial-recall--moment-buffer mapped)))

(defun partial-recall--moment-from-buffer (buffer &optional memory)
  "Get the moment that encapsulates BUFFER.

Searches all memories unless MEMORY is provided."
  (when-let* ((memory (or memory
                          (let ((memories (partial-recall--memories))
                                (find-memory (apply-partially #'partial-recall--memory-buffer-p buffer)))
                            (seq-find find-memory memories))))
              (ring (partial-recall--memory-ring memory))
              (index (partial-recall--moments-member ring buffer)))

    (ring-ref ring index)))

(defun partial-recall--update-count (&optional buffer)
  "Get the update count of BUFFER."
  (when-let* ((buffer (or buffer (current-buffer)))
              (moment (partial-recall--moment-from-buffer buffer)))

    (partial-recall--moment-update-count moment)))

(defun partial-recall--name (memory)
  "Get the name of MEMORY."
  (if (partial-recall--subconscious-p memory)
      partial-recall--subconscious-key
    (partial-recall--tab-name memory)))

(defun partial-recall--tab (memory)
  "Get the tab for MEMORY."
  (when-let ((key (partial-recall--memory-key memory))
             (tabs (funcall tab-bar-tabs-function)))

    (seq-find (lambda (it) (string= key (alist-get 'pr it))) tabs)))

(defun partial-recall--tab-name (&optional memory)
  "Get the tab name for MEMORY."
  (if-let ((memory (or memory (partial-recall--reality)))
           (tab (partial-recall--tab memory)))
      (alist-get 'name tab)
    ""))

(defun partial-recall--memories ()
  "Get all memories."
  (hash-table-values partial-recall--table))

(defun partial-recall--reality ()
  "Get the current memory."
  (if-let* ((table partial-recall--table)
            (key (partial-recall--key))
            (memory (gethash key table)))
      memory
    (let ((new-memory (partial-recall--memory-create key)))

      (when key
        (puthash key new-memory table)
        new-memory))))

(defun partial-recall--subconscious ()
  "Return (or create) the subconscious."
  (if-let* ((table partial-recall--table)
            (key partial-recall--subconscious-key)
            (subconscious (gethash key table)))
      subconscious
    (let ((memory (partial-recall--memory-create key)))

      (puthash key memory partial-recall--table)
      memory)))

(defun partial-recall--lifted (buffer)
  "Lift BUFFER out of the subconscious if present."
  (when-let* ((memory (partial-recall--subconscious))
              (found (partial-recall--remove-buffer buffer memory)))

    (partial-recall--log "Lifting '%s' out of the subconscious" (buffer-name buffer))

    (partial-recall--moment-update-timestamp found)))

(defun partial-recall--moment-set-permanence (moment permanence)
  "Set MOMENT PERMANENCE."
  (setf (partial-recall--moment-permanence moment) permanence)

  moment)

(defun partial-recall--moment-update-timestamp (moment)
  "Update the timestamp for MOMENT."
  (setf (partial-recall--moment-timestamp moment) (floor (time-to-seconds)))

  moment)

(defun partial-recall--moment-increment-count (moment)
  "Increment the update count for MOMENT."
  (let* ((count (partial-recall--moment-update-count moment))
         (updated-count (1+ count)))

    (partial-recall--maybe-implant-moment moment updated-count)

    (setf (partial-recall--moment-update-count moment) updated-count)

    moment))

(defun partial-recall--moment-refresh (moment &optional reset)
  "Refresh MOMENT.

This will update its timestamp and increment its update count. If
RESET is t, reset the update count instead and remove permanence."
  (if reset
      (progn
        (partial-recall--reset-count moment)
        (partial-recall--moment-set-permanence moment nil))
    (partial-recall--moment-update-timestamp moment)
    (partial-recall--moment-increment-count moment)))

(defun partial-recall--reset-count (moment)
  "Reset the update count for MOMENT."
  (setf (partial-recall--moment-update-count moment) 0)

  moment)

(defun partial-recall--remove-buffer (buffer memory)
  "Remove BUFFER from MEMORY and return it."
  (when-let* ((moments (partial-recall--memory-ring memory))
              (index (partial-recall--moments-member moments buffer))
              (removed (ring-remove moments index)))

    removed))

;;; -- Handlers

(defun partial-recall--schedule-buffer (buffer)
  "Schedule handling BUFFER."
  (with-current-buffer buffer
    (and-let* ((buffer (current-buffer))
               (new (not (eq partial-recall--last-checked buffer)))
               ((partial-recall--meaningful-buffer-p buffer)))

      (when partial-recall--timer
        (cancel-timer partial-recall--timer))

      (setq partial-recall--timer
            (run-at-time
             partial-recall-handle-delay
             nil
             #'partial-recall--handle-buffer buffer)))))

(defun partial-recall--handle-buffer (buffer)
  "Handle BUFFER.

This will remember new buffers and maybe reclaim mapped buffers.
If in between scheduling and handling the buffer it can no longer
be found, it will be ignored."
  (when (partial-recall--find-buffer buffer)

    (partial-recall--debug "Found %s" buffer)

    (if (partial-recall--mapped-buffer-p buffer)
        (partial-recall--recollect buffer)
      (partial-recall--remember buffer))

    (setq partial-recall--last-checked buffer)))

(defun partial-recall--after-switch-to-buffer (buffer &optional norecord &rest _)
  "Schedule the BUFFER that was switched to.

Don't do anything if NORECORD is t."
  (unless norecord
    (partial-recall--schedule-buffer buffer)))

(defun partial-recall--on-create (tab)
  "Equip TAB with a unique hash key."
  (let ((key (partial-recall--create-key tab))
        (state (cdr tab)))

    (setcdr tab (push (cons 'pr key) state))))

(defun partial-recall--on-close (tab only)
  "Remove TAB from table if it is not the ONLY one."
  (and-let* (((not only))
             (tab-key (partial-recall--key tab))
             (table partial-recall--table)
             (memory (gethash tab-key table))
             (moments (partial-recall--memory-ring memory)))

    (dolist (it (ring-elements moments))
      (partial-recall--suppress it))

    (remhash tab-key table)))

(defun partial-recall--on-find-file ()
  "Handle finding a file."
  (partial-recall--schedule-buffer (current-buffer)))

(defun partial-recall--on-frame-delete (frame)
  "Clear hashes associated with FRAME."
  (let ((tabs (funcall tab-bar-tabs-function frame)))

    (dolist (tab tabs)
      (partial-recall--on-close tab nil))))

(defun partial-recall--on-minibuffer-setup ()
  "Maybe record the buffer before entry."
  (and-let* (((memq this-command partial-recall-record-triggers))
             (buffer (window-buffer (minibuffer-selected-window))))

    (setq partial-recall--before-minibuffer buffer)))

(defun partial-recall--on-minibuffer-exit ()
  "Delete the recorded buffer."
  (setq partial-recall--before-minibuffer nil))

;;; -- Actions

(defun partial-recall--remember (buffer)
  "Remember the BUFFER for this tab.

This will either create a new moment for the buffer or reclaim
one from the subconscious, assuming no such moment is already
part of the current reality."
  (and-let* ((memory (partial-recall--reality))
             (ring (partial-recall--memory-ring memory))
             ((not (partial-recall--moments-member ring buffer))))

    (partial-recall--probe-memory memory)

    (let ((moment (or (partial-recall--lifted buffer)
                      (partial-recall--moment-create buffer))))

      (ring-insert ring moment))))

(defun partial-recall--reinforce (buffer)
  "Reinforce the BUFFER in reality.

This will re-insert the buffer's moment."
  (and-let* ((reality (partial-recall--reality))
             (moment (partial-recall--moment-from-buffer buffer reality)))

    (partial-recall--reinsert moment reality)))

(defun partial-recall--reclaim (buffer &optional force)
  "Reclaim BUFFER if possible.

If BUFFER is nil, reclaim the current buffer.

If FORCE is t, will reclaim even if it was implanted or the
threshold wasn't passed."
  (and-let* ((reality (partial-recall--reality))
             (owner (partial-recall--buffer-owner buffer))
             ((not (eq reality owner)))
             (moment (partial-recall--moment-from-buffer buffer owner))
             ((or force
                  (and
                   (not (partial-recall--moment-permanence moment))
                   (partial-recall--exceeds-p moment partial-recall-reclaim-min-age)))))

    (partial-recall--swap owner reality moment)))

(defun partial-recall--forget (&optional buffer suppress)
  "Forget BUFFER.

This will remove the buffer's moment from the memory. If SUPPRESS
is t, the forgotten moment goes into the subconscious."
  (let* ((buffer (or buffer (current-buffer)))
         (removed nil)
         (maybe-remove (lambda (key memory)
                         (when-let* (((not removed))
                                     (moment (partial-recall--remove-buffer buffer memory)))

                           (setq removed t)

                           (when (and suppress
                                      (not (string= key partial-recall--subconscious-key)))
                             (partial-recall--suppress moment))

                           (partial-recall--maybe-resize-memory memory)))))

    (partial-recall--clean-up-buffer buffer)

    (maphash maybe-remove partial-recall--table)))

(defun partial-recall--implant (&optional buffer excise)
  "Make BUFFER's moment permanent.

A permanent moment can not be reclaimed and will not be
automatically forgotten.

If EXCISE is t, remove permanence instead."
  (when-let* ((buffer (or buffer (current-buffer)))
              (moment (partial-recall--moment-from-buffer buffer)))

    (unless (eq (partial-recall--moment-permanence moment) (not excise))

      (partial-recall--log "Implanting %s" (partial-recall--moment-buffer moment))

      (partial-recall--moment-set-permanence moment (not excise))
      (partial-recall--moment-increment-count moment))))

(defun partial-recall--suppress (moment)
  "Suppress MOMENT in the subconscious."
  (and-let* ((buffer (partial-recall--moment-buffer moment))
             ((partial-recall--meaningful-buffer-p buffer))
             (memory (partial-recall--subconscious))
             (ring (partial-recall--memory-ring memory)))

    (when (partial-recall--memory-at-capacity-p memory)
      (let* ((removed (ring-remove ring))
             (buffer (partial-recall--moment-buffer removed)))

        (when partial-recall-repress
          (partial-recall--log "Repressing buffer %s" buffer)
          (kill-buffer buffer))))

    (partial-recall--moment-refresh moment t)

    (ring-insert ring moment)))

(defun partial-recall--lift (buffer)
  "Lift BUFFER into reality."
  (when-let* ((moment (partial-recall--lifted buffer))
              (reality (partial-recall--reality))
              (ring (partial-recall--memory-ring reality)))

    (partial-recall--probe-memory reality)

    (ring-insert ring moment)))

(defun partial-recall--recollect (buffer)
  "Recollect the BUFFER.

Recollection happens to mapped buffers. Those belonging to the
current reality are reinforced. Those of other memories
are (potentially) reclaimed."
  (if (partial-recall--reality-buffer-p buffer)
      (partial-recall--reinforce buffer)
    (when partial-recall-reclaim
      (partial-recall--reclaim buffer))))

(defun partial-recall--swap (a b moment)
  "Swap MOMENT from memory A to B.

Both memories will be probed. Memory A after the moment was
removed, memory B before it is inserted."
  (and-let* ((a-ring (partial-recall--memory-ring a))
             (b-ring (partial-recall--memory-ring b))
             (a-tab (partial-recall--name a))
             (b-tab (partial-recall--name b))
             (index (ring-member a-ring moment)))

    (let* ((removed (ring-remove a-ring index))
           (buffer (partial-recall--moment-buffer removed)))

      (partial-recall--log "Swapping '%s' from '%s' to '%s'" (buffer-name buffer) a-tab b-tab)

      (partial-recall--probe-memory a)
      (partial-recall--probe-memory b)

      (partial-recall--moment-refresh moment)

      (ring-insert b-ring removed))))

(defun partial-recall--reinsert (moment memory)
  "Reinsert MOMENT into MEMORY.

This removes, inserts and extends. The moment is refreshed."
  (and-let* ((ring (partial-recall--memory-ring memory))
             (name (partial-recall--name memory))
             ((ring-member ring moment))
             (buffer (partial-recall--moment-buffer moment)))

    (partial-recall--debug "Re-inserting buffer '%s' in '%s'" (buffer-name buffer) name)

    (partial-recall--moment-refresh moment)

    (ring-remove+insert+extend ring moment t)))

(defun partial-recall--clean-up-buffer (buffer)
  "Clean up BUFFER if necessary.

Deletes any window currently displaying it and makes sure it is
no longer recorded as the last checked buffer."
  (when-let* ((windows (window-list)))
    (dolist (window windows)
      (when (eq (window-buffer window) buffer)
        (quit-window nil window))))

  (when (eq partial-recall--last-checked buffer)
    (setq partial-recall--last-checked nil)))

(defun partial-recall--probe-memory (memory)
  "Probe MEMORY.

This will reinsert implanted moments, suppress removed moments,
as well as resize and extend the memory if necessary."
  (partial-recall--maybe-resize-memory memory)
  (partial-recall--maybe-reinsert-implanted memory)
  (partial-recall--maybe-extend-memory memory)
  (partial-recall--maybe-suppress-oldest-moment memory))

(defun partial-recall--maybe-reinsert-implanted (memory)
  "Maybe reinforce oldest moments in MEMORY.

This will loop over the moments in reverse and makes sure to
re-insert any implanted one."
  (and-let* ((ring (partial-recall--memory-ring memory))
             (oldest (partial-recall--ring-oldest ring)))

    (let ((checked nil))

      (while (and (not (memq oldest checked))
                  (partial-recall--moment-permanence oldest))
        (partial-recall--reinsert oldest memory)
        (push oldest checked)
        (setq oldest (partial-recall--ring-oldest ring))))))

(defun partial-recall--maybe-resize-memory (memory)
  "Maybe resize MEMORY if it has grown but could shrink."
  (let ((ring (partial-recall--memory-ring memory))
        (orig (partial-recall--memory-orig-size memory))
        (curr (ring-size (partial-recall--memory-ring memory))))

    (when (and (not (partial-recall--memory-at-capacity-p memory))
               (> curr orig))
      (partial-recall--debug "Resizing %s" (partial-recall--name memory))

      (ring-resize ring (1- (ring-size ring))))))

(defun partial-recall--maybe-extend-memory (memory)
  "Maybe extend MEMORY.

See `partial-recall--should-extend-memory-p'."
  (when (and (partial-recall--memory-at-capacity-p memory)
             (partial-recall--should-extend-memory-p memory))
    (partial-recall--debug "Extending %s" (partial-recall--name memory))

    (ring-extend (partial-recall--memory-ring memory) 1)))

(defun partial-recall--maybe-suppress-oldest-moment (memory)
  "Suppress the oldest moment in MEMORY if necessary.

This will be any moment that would be removed anyway by insertion
beyond the memory's limit."
  (and-let* (((partial-recall--memory-at-capacity-p memory))
             (ring (partial-recall--memory-ring memory))
             ((not (zerop (ring-length ring))))
             (removed (ring-remove ring)))

    (partial-recall--suppress removed)))

(defun partial-recall--maybe-implant-moment (moment count)
  "Check if MOMENT should be implanted automatically.

This is true if COUNT exceeds `partial-recall-auto-implant-threshold'."
  (when (and partial-recall-auto-implant
             (not (partial-recall--moment-permanence moment))
             (> count partial-recall-auto-implant-threshold))

    (partial-recall--implant (partial-recall--moment-buffer moment))))

;;; -- Conditionals

(defun partial-recall--memory-at-capacity-p (memory)
  "Check if MEMORY is at capacity."
  (when-let ((ring (partial-recall--memory-ring memory)))

    (eq (ring-length ring) (ring-size ring))))

(defun partial-recall--memory-buffer-p (buffer memory)
  "Check if BUFFER is a member of MEMORY."
  (partial-recall--moments-member
   (partial-recall--memory-ring memory)
   buffer))

(defun partial-recall--moment-buffer-p (buffer moment)
  "Check if BUFFER is encapsulated by MOMENT."
  (eq (partial-recall--moment-buffer moment) buffer))

(defun partial-recall--moments-member (moments buffer)
  "Check if BUFFER is a member of MOMENTS."
  (catch 'found
    (dotimes (ind (ring-length moments))
      (when (equal buffer (partial-recall--moment-buffer (ring-ref moments ind)))
        (throw 'found ind)))))

(defun partial-recall--mapped-buffer-p (buffer &optional include-subconscious)
  "Check if BUFFER is mapped.

This excludes moments in the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (let ((buffers (partial-recall--mapped-buffers include-subconscious)))

    (memq buffer buffers)))

(defun partial-recall--reality-p (memory)
  "Check if MEMORY is the reality."
  (eq (partial-recall--reality) memory))

(defun partial-recall--reality-buffer-p (buffer)
  "Check if BUFFER belongs to the current memory."
  (when-let* ((reality (partial-recall--reality))
              (moments (partial-recall--memory-ring reality)))

    (partial-recall--moments-member moments buffer)))

(defun partial-recall--reality-owns-buffer-p (&optional buffer)
  "Check if reality owns BUFFER."
  (when-let ((memory (partial-recall--reality)))

    (partial-recall--memory-buffer-p buffer memory)))

(defun partial-recall--should-extend-memory-p (memory)
  "Check if MEMORY should extend its ring size.

This is the case if the oldest ring element is still younger than
the max age."
  (when-let* ((ring (partial-recall--memory-ring memory))
              (to-remove (partial-recall--ring-oldest ring)))

    (partial-recall--falls-below-p to-remove partial-recall-max-age)))

(defun partial-recall--has-buffers-p (&optional memory)
  "Check if the MEMORY has buffers."
  (when-let* ((memory (or memory (partial-recall--reality)))
              (moments (partial-recall--memory-ring memory)))

    (not (ring-empty-p moments))))

(defun partial-recall--subconscious-p (memory)
  "Check if MEMORY is the subconscious."
  (string= partial-recall--subconscious-key (partial-recall--memory-key memory)))

(defun partial-recall--exceeds-p (moment threshold)
  "Check if MOMENT's age exceeds THRESHOLD."
  (< threshold
     (- (floor (time-to-seconds))
        (partial-recall--moment-timestamp moment))))

(defun partial-recall--falls-below-p (moment threshold)
  "Check if MOMENT's age falls below THRESHOLD."
  (> threshold
     (- (floor (time-to-seconds))
        (partial-recall--moment-timestamp moment))))

(defun partial-recall--meaningful-buffer-p (buffer)
  "Check if BUFFER should be remembered."
  (not (null (buffer-file-name buffer))))

;;; -- Utility

(defun partial-recall--warn (message)
  "Warn about MESSAGE."
  (display-warning 'partial-recall message :warning))

(defun partial-recall--log (fmt &rest args)
  "Use ARGS to format FMT if not silenced."
  (when partial-recall-log
    (apply 'message fmt args)))

(defun partial-recall--debug (fmt &rest args)
  "Use ARGS to format FMT if debug is enabled."
  (when (< partial-recall-log-level 1)
    (apply 'partial-recall--log fmt args)))

(defun partial-recall--repr (instance)
  "Print a readable representation of INSTANCE."
  (pcase (type-of instance)

    ('partial-recall--moment
     (let ((buffer (partial-recall--moment-buffer instance))
           (ts (partial-recall--moment-timestamp instance)))

       (format
        "#<moment %s (%s)>"
        (buffer-name buffer)
        (format-time-string "%H:%M:%S" (seconds-to-time ts)))))

    ('partial-recall--memory
     (let ((ring (partial-recall--memory-ring instance))
           (name (partial-recall--name instance)))

       (format
        "#<memory %s (%d/%d)>"
        name
        (ring-length ring)
        (ring-size ring))))

    (unknown (user-error "No representation for %s type" unknown))))

(defun partial-recall--find-buffer (buffer)
  "Find BUFFER in the current frame.

This also checks for buffers that might have been obscured."
  (and buffer
       (buffer-live-p buffer)
       (or (eq buffer partial-recall--before-minibuffer)
           (memq buffer (mapcar #'window-buffer (window-list))))))

;;; -- Completion

(defun partial-recall--complete-dream (prompt)
  "Complete dream buffer using PROMPT."
  (let* ((buffers (partial-recall--mapped-buffers))
         (other-buffers (seq-filter (lambda (it) (not (partial-recall--reality-owns-buffer-p it))) buffers))
         (a (mapcar (lambda (it) (cons (buffer-name it) it)) other-buffers))
         (selection (completing-read prompt a nil t)))

    (cdr-safe (assoc selection a ))))

(defun partial-recall--complete-reality (prompt)
  "Complete reality buffer using PROMPT."
  (let* ((buffers (partial-recall--mapped-buffers))
         (other-buffers (seq-filter #'partial-recall--reality-owns-buffer-p buffers))
         (a (mapcar (lambda (it) (cons (buffer-name it) it)) other-buffers))
         (current (current-buffer))
         (initial (when (memq current other-buffers) (buffer-name current)))
         (selection (completing-read prompt a nil t initial)))

    (cdr-safe (assoc selection a))))

(defun partial-recall--complete-subconscious (prompt)
  "Complete subconscious buffer using PROMPT."
  (let* ((memory (partial-recall--subconscious))
         (ring (partial-recall--memory-ring memory))
         (moments (ring-elements ring))
         (buffers (mapcar #'partial-recall--moment-buffer moments))
         (a (mapcar (lambda (it) (cons (buffer-name it) it)) buffers))
         (selection (completing-read prompt a nil t)))

    (cdr-safe (assoc selection a))))

(defun partial-recall--complete-any (prompt &optional allow-non-file)
  "Complete any buffer using PROMPT.

Mapped buffers and non-file buffers (unless ALLOW-NON-FILE is t)
are not considered."
  (let* ((buffers (if allow-non-file
                      (buffer-list)
                    (seq-filter #'partial-recall--meaningful-buffer-p (buffer-list))))
         (candidates (seq-filter (lambda (it) (not (partial-recall--mapped-buffer-p it t))) buffers))
         (a (mapcar (lambda (it) (cons (buffer-name it) it)) candidates))
         (current (current-buffer))
         (initial (unless (partial-recall--mapped-buffer-p current t)
                    (buffer-name (current-buffer))))
         (selection (completing-read prompt a nil t initial)))

    (cdr-safe (assoc selection a))))

;;; -- Setup

(defun partial-recall--fix-up-primary-tab ()
  "Fix up the primary tab."
  (if-let* ((mode tab-bar-mode)
            (tabs (funcall tab-bar-tabs-function))
            (original (nth 0 tabs)))

      (unless (partial-recall--key original)
        (partial-recall--on-create original))
    (partial-recall--warn "Might have failed to set up original tab")))

(defun partial-recall--queue-fix-up (&rest _r)
  "Queue a fix-up of the original tab."
  (run-at-time 1.0 nil #'partial-recall--fix-up-primary-tab))

(defun partial-recall-mode--setup ()
  "Set up `partial-recall-mode'."
  (unless tab-bar-mode
    (tab-bar-mode 1))

  (partial-recall--queue-fix-up)

  (advice-add
   partial-recall--switch-to-buffer-function :after
   #'partial-recall--after-switch-to-buffer)
  (add-hook 'minibuffer-setup-hook #'partial-recall--on-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'partial-recall--on-minibuffer-exit)
  (add-hook 'find-file-hook #'partial-recall--on-find-file)
  (add-hook 'after-make-frame-functions #'partial-recall--queue-fix-up)
  (add-hook 'kill-buffer-hook #'partial-recall--forget)
  (add-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (add-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)
  (add-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

(defun partial-recall-mode--teardown ()
  "Tear down `partial-recall-mode'."
  (advice-remove
   partial-recall--switch-to-buffer-function
   #'partial-recall--after-switch-to-buffer)
  (remove-hook 'minibuffer-setup-hook #'partial-recall--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'partial-recall--on-minibuffer-exit)
  (remove-hook 'find-file-hook #'partial-recall--on-find-file)
  (remove-hook 'after-make-frame-functions #'partial-recall--queue-fix-up)
  (remove-hook 'kill-buffer-hook #'partial-recall--forget)
  (remove-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (remove-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)
  (remove-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

;;; -- API

;;;###autoload
(defvar partial-recall-command-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "b") 'partial-recall-switch-to-buffer)
    (define-key map (kbd "c") 'partial-recall-reclaim)
    (define-key map (kbd "f") 'partial-recall-forget)
    (define-key map (kbd "i") 'partial-recall-implant)
    (define-key map (kbd "i") 'partial-recall-reinforce)
    (define-key map (kbd "l") 'partial-recall-menu)
    (define-key map (kbd "m") 'partial-recall-remember)
    (define-key map (kbd "s") 'partial-recall-lift)
    map)
  "Map for `partial-recall-mode' commands.")

;;;###autoload
(define-minor-mode partial-recall-mode
  "Keep track of buffers opened in a tab."
  :lighter partial-recall-mode-lighter
  :global t
  (if partial-recall-mode
      (partial-recall-mode--setup)
    (partial-recall-mode--teardown)))

;;;###autoload
(defun partial-recall-remember (buffer)
  "Remember BUFFER.

The buffer is selected from a list of unmapped file buffers. If
called with a prefix argument, the selection will be widened to
all buffers."
  (interactive (list (partial-recall--complete-any "Remember buffer: " current-prefix-arg)))

  (partial-recall--remember buffer)

  (funcall partial-recall--switch-to-buffer-function buffer))

;;;###autoload
(defun partial-recall-switch-to-buffer (buffer)
  "Switch to BUFFER."
  (interactive (list (partial-recall--complete-reality "Switch to moment: ")))

  (switch-to-buffer buffer))

;;;###autoload
(defun partial-recall-reinforce (buffer)
  "Reinforce BUFFER."
  (interactive (list (partial-recall--complete-reality "Re-inforce moment: ")))

  (partial-recall--reinforce buffer))

;;;###autoload
(defun partial-recall-reclaim (buffer)
  "Reclaim BUFFER.

This will always force-reclaim."
  (interactive (list (partial-recall--complete-dream "Reclaim moment: ")))

  (partial-recall--reclaim buffer t))

;;;###autoload
(defun partial-recall-forget (buffer)
  "Forget BUFFER."
  (interactive (list (partial-recall--complete-reality "Forget moment: ")))

  (partial-recall--forget buffer t))

;;;###autoload
(defun partial-recall-implant (buffer &optional excise)
  "Implant the BUFFER.

If EXCISE is T, do that instead."
  (interactive (list (partial-recall--complete-reality (if current-prefix-arg
                                                           "Excise moment: "
                                                         "Implant moment: "))
                     current-prefix-arg))

  (partial-recall--implant buffer excise))

;;;###autoload
(defun partial-recall-lift (buffer)
  "Lift BUFFER out of the subconscious."
  (interactive (list (partial-recall--complete-subconscious "Lift moment: ")))

  (partial-recall--lift buffer))

(provide 'partial-recall)

;;; partial-recall.el ends here

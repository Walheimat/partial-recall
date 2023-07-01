;;; partial-recall.el --- STM for `tab-bar-mode' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.5.0
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

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)

;; -- Customization

(defgroup partial-recall nil
  "Short-term (buffer) memory."
  :group 'partial-recall)

(defcustom partial-recall-buffer-limit 10
  "The amount of buffers to recall.

This limit of a memory may increase if buffers are remembered in
quick succession. See `partial-recall-max-age'."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-max-age (* 5 60)
  "Threshold in seconds that will allow a memory to grow.

If the oldest moment is younger than the threshold, the limit is
increased and the buffer will remain."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-reclaim t
  "Whether to automatically reclaim buffers from other memories."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-reclaim-min-age (* 10 60)
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

(defcustom partial-recall-auto-implant-threshold 10
  "Minimum of updates before auto-implanting."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-mode-lighter " pr"
  "The mode line lighter."
  :type 'string
  :group 'partial-recall)

;; -- Internal variables

(defvar partial-recall--table (make-hash-table))
(defvar partial-recall--subconscious-key "subconscious")
(defvar partial-recall--timer nil)
(defvar partial-recall--last-checked nil)
(defvar partial-recall--log nil)

;; -- Structures

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

;; -- Accessors

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
         (memories (hash-table-values partial-recall--table))
         (find (apply-partially #'partial-recall--memory-buffer-p buffer)))

    (seq-find find memories)))

(defun partial-recall--mapped-buffers ()
  "Get all mapped buffers."
  (let ((mapped (cl-loop for k being the hash-keys of partial-recall--table
                         using (hash-values memory)
                         unless (string= k partial-recall--subconscious-key)
                         append (ring-elements (partial-recall--memory-ring memory)))))

    (mapcar #'partial-recall--moment-buffer mapped)))

(defun partial-recall--moment-from-buffer (buffer)
  "Get the moment that encapsulates BUFFER."
  (when-let* ((memories (hash-table-values partial-recall--table))
              (find-memory (apply-partially #'partial-recall--memory-buffer-p buffer))
              (memory (seq-find find-memory memories))
              (ring (partial-recall--memory-ring memory))
              (index (partial-recall--moments-member ring buffer)))

    (ring-ref ring index)))

(defun partial-recall--update-count (&optional buffer)
  "Get the update count of BUFFER."
  (when-let* ((buffer (or buffer (current-buffer)))
              (reality (partial-recall--reality))
              (moments (partial-recall--memory-ring reality))
              (index (partial-recall--moments-member moments buffer))
              (moment (ring-ref moments index)))

    (partial-recall--moment-update-count moment)))

(defun partial-recall--name (memory)
  "Get the name of MEMORY."
  (if (partial-recall--subconscious-p memory)
      "subconscious"
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
              (moments (partial-recall--memory-ring memory))
              (index (partial-recall--moments-member moments buffer))
              (found (ring-remove moments index)))

    (partial-recall--log "Lifting '%s' out of the subconscious" (buffer-name buffer))

    (partial-recall--moment-update-timestamp found)
    found))

(defun partial-recall--moment-set-permanence (moment permanence)
  "Set MOMENT PERMANENCE."
  (setf (partial-recall--moment-permanence moment) permanence))

(defun partial-recall--moment-update-timestamp (moment)
  "Update the timestamp for MOMENT."
  (setf (partial-recall--moment-timestamp moment) (floor (time-to-seconds))))

(defun partial-recall--moment-increment-count (moment)
  "Increment the update count for MOMENT."
  (let* ((count (partial-recall--moment-update-count moment))
         (updated-count (1+ count)))

    (partial-recall--maybe-implant-moment moment updated-count)

    (setf (partial-recall--moment-update-count moment) updated-count)))

(defun partial-recall--reset-count (moment)
  "Reset the update count for MOMENT."
  (setf (partial-recall--moment-update-count moment) 0))

;; -- Handlers

(defun partial-recall--handle-buffer (buffer)
  "Handle BUFFER.

This will remember new buffers and maybe reclaim mapped buffers."
  (when (buffer-live-p buffer)
    (setq partial-recall--last-checked buffer)

    (if (partial-recall--mapped-buffer-p buffer)
        (partial-recall--recollect buffer)
      (partial-recall--remember buffer))))

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

(defun partial-recall--on-buffer-list-update ()
  "Schedule handling the current buffer."
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

;; -- Actions

(defun partial-recall--remember (buffer)
  "Remember the BUFFER for this tab."
  (when-let* ((memory (partial-recall--reality))
              (ring (partial-recall--memory-ring memory)))

    (unless (partial-recall--moments-member ring buffer)
      (partial-recall--maybe-reinforce-implanted memory)

      (partial-recall--maybe-extend-memory memory)

      (let ((moment (or (partial-recall--lifted buffer)
                        (partial-recall--moment-create buffer))))

        (ring-insert ring moment)))))

(defun partial-recall--reinforce (buffer &optional force)
  "Reinforce the BUFFER.

If the buffer is close to being lost to the memory, it is
re-inserted and its timestamp updated.

If FORCE is t, re-insertion and update will always be performed."
  (and-let* ((reality (partial-recall--reality))
             ((or force (partial-recall--memory-at-capacity-p reality)))
             (moments (partial-recall--memory-ring reality))
             (index (partial-recall--moments-member moments buffer))
             (moment (ring-ref moments index)))

    (partial-recall--reinsert moment reality)))

(defun partial-recall--reclaim (buffer &optional force)
  "Reclaim BUFFER if possible.

If BUFFER is nil, reclaim the current buffer.

If FORCE is t, will reclaim even if the threshold wasn't passed."
  (and-let* ((reality (partial-recall--reality))
             (owner (partial-recall--buffer-owner buffer))
             ((not (eq reality owner)))
             (ring (partial-recall--memory-ring owner))
             (find (apply-partially #'partial-recall--moment-buffer-p buffer))
             (moment (seq-find find (ring-elements ring)))
             ((or force
                  (< partial-recall-reclaim-min-age
                     (- (floor (time-to-seconds))
                        (partial-recall--moment-timestamp moment))))))

    (partial-recall--swap owner reality moment)))

(defun partial-recall--forget (&optional buffer suppress)
  "Forget BUFFER.

If SUPPRESS is t, do that."
  (let* ((buffer (or buffer (current-buffer)))
         (removed nil)
         (maybe-remove (lambda (key memory)
                         (when-let* (((not removed))
                                     (ring (partial-recall--memory-ring memory))
                                     (index (partial-recall--moments-member ring buffer))
                                     (moment (ring-remove ring index)))

                           (setq removed t)

                           (when (and suppress
                                      (not (string= key partial-recall--subconscious-key)))
                             (partial-recall--suppress moment))

                           (partial-recall--maybe-resize-memory memory)))))

    (maphash maybe-remove partial-recall--table)))

(defun partial-recall--implant (&optional buffer excise)
  "Make BUFFER permanent.

If EXCISE is t, remove permanence instead."
  (when-let* ((buffer (or buffer (current-buffer)))
              (owner (partial-recall--buffer-owner buffer))
              (ring (partial-recall--memory-ring owner))
              (find (apply-partially #'partial-recall--moment-buffer-p buffer))
              (moment (seq-find find (ring-elements ring))))

    (unless (eq (partial-recall--moment-permanence moment) (not excise))
      (partial-recall--moment-set-permanence moment (not excise))
      (partial-recall--moment-increment-count moment))))

(defun partial-recall--suppress (moment)
  "Suppress MOMENT in the subconscious."
  (when-let* ((memory (partial-recall--subconscious))
              (ring (partial-recall--memory-ring memory)))

    (when (partial-recall--memory-at-capacity-p memory)
      (let* ((removed (ring-remove ring))
             (buffer (partial-recall--moment-buffer removed)))

        (when partial-recall-repress
          (kill-buffer buffer))))

    (partial-recall--reset-count moment)
    (partial-recall--moment-set-permanence moment nil)

    (ring-insert ring moment)))

(defun partial-recall--lift (buffer)
  "Lift BUFFER into reality."
  (when-let* ((moment (partial-recall--lifted buffer))
              (reality (partial-recall--reality))
              (ring (partial-recall--memory-ring reality)))

    (ring-insert ring moment)))

(defun partial-recall--recollect (buffer)
  "Recollect the BUFFER."
  (if (partial-recall--reality-buffer-p buffer)
      (partial-recall--reinforce buffer)
    (when partial-recall-reclaim
      (partial-recall--reclaim buffer))))

(defun partial-recall--swap (a b moment)
  "Swap MOMENT from memory A to B."
  (and-let* ((a-ring (partial-recall--memory-ring a))
             (b-ring (partial-recall--memory-ring b))
             (a-tab (partial-recall--name a))
             (b-tab (partial-recall--name b))
             (index (ring-member a-ring moment)))

    (let* ((removed (ring-remove a-ring index))
           (buffer (partial-recall--moment-buffer removed)))

      (partial-recall--log "Swapping '%s' from '%s' to '%s'" (buffer-name buffer) a-tab b-tab)

      (partial-recall--maybe-reinforce-implanted b)

      (when (and (partial-recall--memory-at-capacity-p b)
                 (partial-recall--should-extend-memory-p b))
        (ring-extend b-ring 1))

      (ring-insert b-ring removed)

      (partial-recall--moment-update-timestamp moment))))

(defun partial-recall--reinsert (moment memory)
  "Re-insert MOMENT into MEMORY."
  (when-let* ((ring (partial-recall--memory-ring memory))
              (index (ring-member ring moment))
              (moment (ring-ref ring index))
              (buffer (partial-recall--moment-buffer moment)))

    (partial-recall--log "Re-inserting buffer '%s'" (buffer-name buffer))

    (ring-remove+insert+extend ring moment t)

    (partial-recall--moment-update-timestamp moment)
    (partial-recall--moment-increment-count moment)))

(defun partial-recall--maybe-reinforce-implanted (memory)
  "Maybe reinforce oldest moment in MEMORY."
  (and-let* ((ring (partial-recall--memory-ring memory))
             (oldest (partial-recall--ring-oldest ring))
             ((partial-recall--moment-permanence oldest)))

    (partial-recall--reinsert oldest memory)))

(defun partial-recall--maybe-resize-memory (memory)
  "Maybe resize MEMORY if it has grown but could shrink."
  (let ((ring (partial-recall--memory-ring memory))
        (orig (partial-recall--memory-orig-size memory))
        (curr (ring-size (partial-recall--memory-ring memory))))

    (when (and (not (partial-recall--memory-at-capacity-p memory))
               (> curr orig))
      (partial-recall--log "Resizing %s" (partial-recall--name memory))

      (ring-resize ring (1- (ring-size ring))))))

(defun partial-recall--maybe-extend-memory (memory)
  "Maybe extend MEMORY."
  (when (and (partial-recall--memory-at-capacity-p memory)
             (partial-recall--should-extend-memory-p memory))
    (partial-recall--log "Extending %s" (partial-recall--name memory))

    (ring-extend (partial-recall--memory-ring memory) 1)))

(defun partial-recall--maybe-implant-moment (moment count)
  "Check if MOMENT should be implanted automatically.

This is true if COUNT exceeds `partial-recall-auto-implant-threshold'."
  (when (and partial-recall-auto-implant
             (> count partial-recall-auto-implant-threshold))

    (partial-recall--log "Implanting %s" (partial-recall--moment-buffer moment))

    (partial-recall--moment-set-permanence moment t)))

;; -- Conditionals

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

(defun partial-recall--mapped-buffer-p (buffer)
  "Check if BUFFER is mapped."
  (let ((buffers (partial-recall--mapped-buffers)))

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

    (> partial-recall-max-age
       (- (floor (time-to-seconds))
          (partial-recall--moment-timestamp to-remove)))))

(defun partial-recall--has-buffers-p (&optional memory)
  "Check if the MEMORY has buffers."
  (when-let* ((memory (or memory (partial-recall--reality)))
              (moments (partial-recall--memory-ring memory)))

    (not (ring-empty-p moments))))

(defun partial-recall--subconscious-p (memory)
  "Check if MEMORY is the subconscious."
  (string= partial-recall--subconscious-key (partial-recall--memory-key memory)))

;; -- Utility

(defun partial-recall--warn (message)
  "Warn about MESSAGE."
  (display-warning 'partial-recall message :warning))

(defun partial-recall--log (fmt &rest args)
  "Use ARGS to format FMT if not silenced."
  (when partial-recall--log
    (apply 'message fmt args)))

(defun partial-recall-toggle-logging ()
  "Toggle logging certain actions."
  (interactive)

  (setq partial-recall--log (not partial-recall--log)))

;; -- Completion

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

;; -- Setup

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

;; -- API

;;;###autoload
(defvar partial-recall-command-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "b") 'partial-recall-switch-to-buffer)
    (define-key map (kbd "r") 'partial-recall-reinforce)
    (define-key map (kbd "c") 'partial-recall-reclaim)
    (define-key map (kbd "f") 'partial-recall-forget)
    (define-key map (kbd "l") 'partial-recall-lift)
    (define-key map (kbd "m") 'partial-recall-menu)
    (define-key map (kbd "i") 'partial-recall-implant)
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
(defun partial-recall-switch-to-buffer (buffer)
  "Switch to BUFFER."
  (interactive (list (partial-recall--complete-reality "Switch to moment: ")))

  (switch-to-buffer buffer))

;;;###autoload
(defun partial-recall-reinforce (buffer)
  "Reinforce BUFFER."
  (interactive (list (partial-recall--complete-reality "Re-inforce moment: ")))

  (partial-recall--reinforce buffer t))

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
  (interactive (list (partial-recall--complete-reality "Implant moment: ")
                     current-prefix-arg))

  (partial-recall--implant buffer excise))

;;;###autoload
(defun partial-recall-lift (buffer)
  "Lift BUFFER out of the subconscious."
  (interactive (list (partial-recall--complete-subconscious "Lift moment: ")))

  (partial-recall--lift buffer))

(provide 'partial-recall)

;;; partial-recall.el ends here

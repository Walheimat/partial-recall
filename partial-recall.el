;;; partial-recall.el --- STM for `tab-bar-mode' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.4.0
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

(defun partial-recall--create-key (tab)
  "Create the key for TAB.

This uses a message digest of the tab, a random number, the Emacs
PID and `recent-keys' vector."
  (let ((object (format "%s%s%s%s" tab (random) (emacs-pid) (recent-keys))))

    (md5 object)))

;; Structures

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

(defun partial-recall--memory-at-capacity-p (memory)
  "Check if MEMORY is at capacity."
  (when-let ((ring (partial-recall--memory-ring memory)))

    (eq (ring-length ring) (ring-size ring))))

(defun partial-recall--mapped-buffers ()
  "Get all mapped buffers."
  (let ((mapped (cl-loop for _k being the hash-keys of partial-recall--table
                         using (hash-values memory)
                         append (ring-elements (partial-recall--memory-ring memory)))))

    (mapcar #'partial-recall--moment-buffer mapped)))

(defun partial-recall--mapped-buffer-p (buffer)
  "Check if BUFFER is mapped."
  (let ((buffers (partial-recall--mapped-buffers)))

    (memq buffer buffers)))

(defun partial-recall--memory-buffer-p (memory buffer)
  "Check if MEMORY owns BUFFER."
  (partial-recall--moments-member
   (partial-recall--memory-ring memory)
   buffer))

(defun partial-recall--moment-buffer-p (moment buffer)
  "Check if MOMENT owns BUFFER."
  (eq (partial-recall--moment-buffer moment) buffer))

(defun partial-recall--moments-member (moments buffer)
  "Check if BUFFER is a member of MOMENTS."
  (catch 'found
    (dotimes (ind (ring-length moments))
      (when (equal buffer (partial-recall--moment-buffer (ring-ref moments ind)))
        (throw 'found ind)))))

(defun partial-recall--moment-set-permanence (moment permanence)
  "Set MOMENT PERMANENCE."
  (setf (partial-recall--moment-permanence moment) permanence)
  (partial-recall--moment-increment-count moment))


(defun partial-recall--moment-update-timestamp (moment)
  "Update the timestamp for MOMENT."
  (setf (partial-recall--moment-timestamp moment) (floor (time-to-seconds)))
  (partial-recall--moment-increment-count moment))

(defun partial-recall--moment-increment-count (moment)
  "Increment the update count for MOMENT."
  (let ((count (partial-recall--moment-update-count moment)))

    (setf (partial-recall--moment-update-count moment) (1+ count))))

;; Helpers

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

(defun partial-recall--reality-buffer-p (buffer)
  "Check if BUFFER belongs to the current memory."
  (when-let* ((reality (partial-recall--reality))
              (moments (partial-recall--memory-ring reality)))

    (partial-recall--moments-member moments buffer)))

(defun partial-recall--reality-owns-buffer-p (&optional buffer)
  "Check if reality owns BUFFER."
  (when-let ((memory (partial-recall--reality)))

    (partial-recall--memory-buffer-p memory buffer)))

(defun partial-recall--should-extend-memory-p (memory)
  "Check if MEMORY should extend its ring size.

This is the case if the oldest ring element is still younger than
the max age."
  (when-let* ((ring (partial-recall--memory-ring memory))
              (to-remove (partial-recall--ring-oldest ring)))

    (> partial-recall-max-age
       (- (floor (time-to-seconds))
          (partial-recall--moment-timestamp to-remove)))))

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
      (ring-resize ring (1- (ring-size ring))))))

(defun partial-recall--maybe-extend-memory (memory)
  "Maybe extend MEMORY."
  (when (and (partial-recall--memory-at-capacity-p memory)
             (partial-recall--should-extend-memory-p memory))
    (ring-extend (partial-recall--memory-ring memory) 1)))

(defun partial-recall--has-buffers-p (&optional memory)
  "Check if the MEMORY has buffers."
  (when-let* ((memory (or memory (partial-recall--reality)))
              (moments (partial-recall--memory-ring memory)))

    (not (ring-empty-p moments))))

(defun partial-recall--buffer-owner (&optional buffer)
  "Return the memory that owns BUFFER.

Defaults to the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (memories (hash-table-values partial-recall--table)))

    (seq-find (lambda (it) (partial-recall--memory-buffer-p it buffer)) memories)))

(defun partial-recall--reinsert (moment memory)
  "Re-insert MOMENT into MEMORY."
  (when-let* ((ring (partial-recall--memory-ring memory))
              (index (ring-member ring moment)))

    (ring-remove+insert+extend ring (ring-ref ring index) t)
    (partial-recall--moment-update-timestamp moment)))

;; Completion

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
         (selection (completing-read prompt a nil t)))

    (cdr-safe (assoc selection a ))))

(defun partial-recall--tab (memory)
  "Get the tab for MEMORY."
  (when-let ((key (partial-recall--memory-key memory))
             (tabs (funcall tab-bar-tabs-function)))

    (seq-find (lambda (it) (string= key (alist-get 'pr it))) tabs)))

(defun partial-recall--tab-name (&optional memory)
  "Get the tab name for MEMORY."
  (when-let ((memory (or memory (partial-recall--reality)))
             (tab (partial-recall--tab memory)))

    (alist-get 'name tab)))

(defun partial-recall--update-count (&optional buffer)
  "Get the update count of BUFFER."
  (when-let* ((buffer (or buffer (current-buffer)))
              (reality (partial-recall--reality))
              (moments (partial-recall--memory-ring reality))
              (index (partial-recall--moments-member moments buffer))
              (moment (ring-ref moments index)))

    (partial-recall--moment-update-count moment)))

;; Utility

(defun partial-recall--ring-oldest (ring)
  "Get the oldest element in RING."
  (and-let* ((length (ring-length ring))
             ((> length 0)))

    (ring-ref ring (1- length))))

(defun partial-recall--warn (message)
  "Warn about MESSAGE."
  (display-warning 'partial-recall message :warning))

(defvar partial-recall--log nil)
(defun partial-recall--log (fmt &rest args)
  "Use ARGS to format FMT if not silenced."
  (when partial-recall--log
    (apply 'message fmt args)))

(defun partial-recall-toggle-logging ()
  "Toggle logging certain actions."
  (interactive)

  (setq partial-recall--log (not partial-recall--log)))

;; Handlers

(defvar partial-recall--last-checked nil)

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

(defvar partial-recall--timer nil)

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

(defun partial-recall--remember (buffer)
  "Remember the BUFFER for this tab."
  (when-let* ((memory (partial-recall--reality))
              (ring (partial-recall--memory-ring memory)))

    (unless (partial-recall--moments-member ring buffer)
      (partial-recall--maybe-reinforce-implanted memory)

      (partial-recall--maybe-extend-memory memory)

      (let ((moment (or (partial-recall--lift buffer)
                        (partial-recall--moment-create buffer))))

        (ring-insert ring moment)))))

(defun partial-recall--swap (a b moment)
  "Swap MOMENT from memory A to B."
  (and-let* ((a-ring (partial-recall--memory-ring a))
             (b-ring (partial-recall--memory-ring b))
             (index (ring-member a-ring moment)))

    (let ((removed (ring-remove a-ring index)))

      (partial-recall--maybe-reinforce-implanted b)

      (when (and (partial-recall--memory-at-capacity-p b)
                 (partial-recall--should-extend-memory-p b))
        (ring-extend b-ring 1))

      (ring-insert b-ring removed)

      (partial-recall--moment-update-timestamp moment))))

(defun partial-recall--recollect (buffer)
  "Recollect the BUFFER."
  (if (partial-recall--reality-buffer-p buffer)
      (partial-recall--reinforce buffer)
    (when partial-recall-reclaim
      (partial-recall--reclaim buffer))))

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

    (partial-recall--log "Reinforcing buffer '%s'" (buffer-name buffer))

    (partial-recall--reinsert moment reality)))

(defun partial-recall--reclaim (buffer &optional force)
  "Reclaim BUFFER if possible.

If BUFFER is nil, reclaim the current buffer.

If FORCE is t, will reclaim even if the threshold wasn't passed."
  (and-let* ((reality (partial-recall--reality))
             (owner (partial-recall--buffer-owner buffer))
             ((not (eq reality owner)))
             (ring (partial-recall--memory-ring owner))
             (moment (seq-find (lambda (it) (partial-recall--moment-buffer-p it buffer)) (ring-elements ring)))
             ((or force
                  (< partial-recall-reclaim-min-age
                     (- (floor (time-to-seconds))
                        (partial-recall--moment-timestamp moment))))))

    (partial-recall--log "Reclaiming '%s' from '%s'" (buffer-name buffer) (partial-recall--tab-name owner))

    (partial-recall--swap owner reality moment)))

(defun partial-recall--forget (&optional buffer suppress)
  "Forget BUFFER.

If SUPPRESS is t, do that."
  (let* ((buffer (or buffer (current-buffer)))
         (table partial-recall--table)
         (subconscious (partial-recall--ensure-subconscious))
         (maybe-remove (lambda (_key memory)
                         (when-let* ((ring (partial-recall--memory-ring memory))
                                     (index (partial-recall--moments-member ring buffer)))

                           (let ((moment (ring-remove ring index)))

                             (when suppress
                               (partial-recall--suppress moment)))

                           (partial-recall--maybe-resize-memory memory)))))

    (when-let ((moment (partial-recall--memory-buffer-p subconscious buffer)))

      (ring-remove (partial-recall--memory-ring subconscious) moment))

    (maphash maybe-remove table)))

(defun partial-recall--implant (&optional buffer excise)
  "Make BUFFER permanent.

If EXCISE is t, remove permanence instead."
  (when-let* ((buffer (or buffer (current-buffer)))
              (owner (partial-recall--buffer-owner buffer))
              (ring (partial-recall--memory-ring owner))
              (moment (seq-find (lambda (it) (partial-recall--moment-buffer-p it buffer)) (ring-elements ring))))

    (unless (eq (partial-recall--moment-permanence moment) (not excise))
      (partial-recall--moment-set-permanence moment (not excise)))))

;; Subconscious

(defvar partial-recall--subconscious-key "subconscious")

(defvar partial-recall--subconscious nil
  "The subconscious.")

(defun partial-recall--ensure-subconscious ()
  "Return (or create) the subconscious."
  (unless partial-recall--subconscious
    (setq partial-recall--subconscious (partial-recall--memory-create partial-recall--subconscious-key)))

  partial-recall--subconscious)

(defun partial-recall--suppress (moment)
  "Suppress MOMENT in the subconscious."
  (when-let* ((memory (partial-recall--ensure-subconscious))
              (ring (partial-recall--memory-ring memory)))

    (when (partial-recall--memory-at-capacity-p memory)
      (let* ((removed (ring-remove ring))
             (buffer (partial-recall--moment-buffer removed)))

        (when partial-recall-repress
          (kill-buffer buffer))))

    (ring-insert ring moment)))

(defun partial-recall--lift (buffer)
  "Lift BUFFER out of the subconscious if present."
  (when-let* ((memory (partial-recall--ensure-subconscious))
              (moments (partial-recall--memory-ring memory))
              (index (partial-recall--moments-member moments buffer))
              (found (ring-remove moments index)))

    (partial-recall--moment-update-timestamp found)
    found))

;; Setup

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

;; API

;;;###autoload
(defvar partial-recall-command-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "b") 'partial-recall-switch-to-buffer)
    (define-key map (kbd "r") 'partial-recall-reinforce)
    (define-key map (kbd "c") 'partial-recall-reclaim)
    (define-key map (kbd "f") 'partial-recall-forget)
    (define-key map (kbd "l") 'partial-recall-menu)
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

(provide 'partial-recall)

;;; partial-recall.el ends here

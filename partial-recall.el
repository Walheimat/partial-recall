;;; partial-recall.el --- STM for `tab-bar-mode' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.8.6
;; Package-Requires: ((emacs "29.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Short-term (buffer) memory for `tab-bar-mode' tabs.
;;
;;`partial-recall' will keep track of meaningful buffers opened in a
;; tab in a ring. A buffer is meaningful if it satisfies user-defined
;; traits. The aforementioned rings are called memories in the context
;; of this package; the current memory is called reality, all other
;; memories are dreams. Buffers are time-stamped to (1) allow for the
;; memory to grow if the oldest one is still relatively young and (2)
;; to allow reclaiming buffers from other memories if they're
;; relatively old. Buffers are called moments in the context of this
;; package.
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

(defcustom partial-recall-memory-size 10
  "The amount of buffers to recall.

This limit of a memory may increase if buffers are remembered in
quick succession. See `partial-recall-max-age'."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-max-age (* 20 60)
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

Has no effect if function `partial-recall-reclaim' is nil."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-repress t
  "Whether `partial-recall-suppress' may kill buffers.

These are buffers that are removed from the subconscious."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-auto-implant 10
  "The amount of focus before auto-implanting a moment.

If this is nil, never auto-implant."
  :type '(choice (const :tag "Don't auto-implant" nil)
                 (integer :tag "Minimum focus before auto-implanting"))
  :group 'partial-recall)

(defcustom partial-recall-auto-switch t
  "Whether to automatically switch to a buffer's memory."
  :type '(choice (const :tag "Don't switch" nil)
                 (const :tag "Switch" t)
                 (const :tag "Prompt first" prompt))
  :group 'partial-recall)

(defcustom partial-recall-lighter '(" "
                                    partial-recall-lighter-title
                                    "["
                                    partial-recall-lighter-moment
                                    "/"
                                    partial-recall-lighter-memory
                                    "]")
  "The lighter as a list of mode line constructs."
  :type '(repeat (choice string symbol))
  :group 'partial-recall)

(defcustom partial-recall-lighter-prefix "pr"
  "The prefix used in `partial-recall-lighter'."
  :type 'string
  :group 'partial-recall)

(defcustom partial-recall-record-triggers '(consult-buffer)
  "Commands that should trigger recording the buffer."
  :type '(repeat symbol)
  :group 'partial-recall)

(defcustom partial-recall-log nil
  "Whether to log.

This is either nil meaning no logging, or 1 for info logging and
1 for info logging."
  :type '(choice (const :tag "No logging" nil)
                 (const :tag "Info" 1)
                 (const :tag "Debug" 0))
  :group 'partial-recall)

(defcustom partial-recall-log-prefix "PR"
  "The prefix used for log messages."
  :type '(choice (const :tag "Text" "PR")
                 (const :tag "None" nil))
  :group 'partial-recall)

(defcustom partial-recall-filter '("COMMIT_EDITMSG" "git-rebase-todo")
  "Names of buffers that should be ignored."
  :type '(repeat regexp)
  :group 'partial-recall)

(defcustom partial-recall-meaningful-traits '(buffer-file-name
                                              partial-recall--not-filtered-p
                                              partial-recall--not-in-view-mode-p)
  "List of functions that describe traits of a meaningful buffer.

These functions are inspected using `func-arity'. If they have a
minimum arity of at least 1 OR the symbol `many' for their
maximum arity, they will be called with `current-buffer' as the
first argument, otherwise they are called with no argument.

If any such function does not return a truthy value, the buffer
is not considered meaningful."
  :type '(repeat function)
  :group 'partial-recall)

(make-obsolete-variable 'partial-recall-traits 'partial-recall-meaningful-traits "0.8.2")

(defcustom partial-recall-memorable-traits '(partial-recall--gracedp)
  "List of functions that determine a memorable moment.

These functions are called with moments up for suppression and
the current prefix argument.

If any such function does return a truthy value, the moment is
considered memorable."
  :type '(repeat function)
  :group 'partial-recall)

(defcustom partial-recall-intensities '((swap . 1) (reinsert . 2) (concentrate . 3))
  "The amount of focus gained from actions swap, reinsert and focus."
  :type '(alist :key-type symbol :value-type integer)
  :group 'partial-recall)

;;; -- Internal variables

(defvar partial-recall--table (make-hash-table :test #'equal))
(defconst partial-recall--subconscious-key "subconscious")

(defvar partial-recall--timer nil)

(defvar partial-recall--concentration-timer nil)
(defconst partial-recall--concentration-repeat 60)

(defvar partial-recall--last-focus nil)

(defvar partial-recall--last-checked nil)
(defvar partial-recall--neglect nil)

(defvar partial-recall--switch-to-buffer-function #'switch-to-buffer)
(defvar partial-recall--pop-to-buffer-function #'pop-to-buffer)

(defvar partial-recall--before-minibuffer nil)

(defvar-local partial-recall--implanted nil)

;;;###autoload
(defvar partial-recall-command-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "b") 'partial-recall-switch-to-buffer)
    (define-key map (kbd "c") 'partial-recall-reclaim)
    (define-key map (kbd "f") 'partial-recall-forget)
    (define-key map (kbd "k") 'partial-recall-forget-some)
    (define-key map (kbd "i") 'partial-recall-implant)
    (define-key map (kbd "j") 'partial-recall-reject)
    (define-key map (kbd "m") 'partial-recall-menu)
    (define-key map (kbd "n") 'partial-recall-next)
    (define-key map (kbd "p") 'partial-recall-previous)
    (define-key map (kbd "u") 'partial-recall-meld)
    (define-key map (kbd "r") 'partial-recall-remember)
    (define-key map (kbd "l") 'partial-recall-lift)
    (define-key map (kbd "x") 'partial-recall-flush)
    (define-key map (kbd "o") 'partial-recall-explain-omission)
    map)
  "Map for `partial-recall-mode' commands.")

(defvar-keymap partial-recall-navigation-map
  :doc "Keymap to repeat navigation commands."
  :repeat t
  "n" 'partial-recall-next
  "p" 'partial-recall-previous)

(defface partial-recall-emphasis
  '((t (:inherit (mode-line-emphasis))))
  "Face used for emphasis."
  :group 'partial-recall)

(defface partial-recall-deemphasized
  '((t (:inherit (shadow))))
  "Face used to de-emphasize."
  :group 'partial-recall)

(defface partial-recall-contrast
  '((t (:inherit (warning))))
  "Face used for contrast."
  :group 'partial-recall)

;;; -- Hooks

(defvar partial-recall-probe-hook nil
  "Functions called after a memory was probed.")

(defvar partial-recall-permanence-change-hook nil
  "Functions called after a moment's permanence has changed.")

(defvar partial-recall-after-insert-hook nil
  "Functions called after a moment was inserted.")

;;; -- Structures

(cl-defstruct (partial-recall--moment
               (:constructor partial-recall--moment-create
                             (buffer
                              &aux
                              (timestamp (floor (time-to-seconds)))
                              (focus 0)
                              (permanence nil))))
  "A moment of partial recall.

A moment is defined by a buffer, a timestamp when that buffer was
first remembered, a count of how many times it was updated and a
permanence marker that can prevent it from being forgotten.

The timestamp is distinct from `buffer-display-time' and the
focus is distinct from `buffer-display-count'."
  buffer timestamp focus permanence)

(cl-defstruct (partial-recall--memory
               (:constructor partial-recall--memory-create
                             (key
                              &aux
                              (ring (make-ring partial-recall-memory-size))
                              (orig-size partial-recall-memory-size))))
  "A memory of partial recall.

A memory is a key that connects it to the hash table, a ring of
moments and the size it had upon construction."
  key ring orig-size)

;;; -- Accessors

(defun partial-recall--key (&optional tab)
  "Get the hash key of TAB."
  (when-let* ((tab (or tab (tab-bar--current-tab))))

    (alist-get 'pr tab)))

(defun partial-recall--create-key (tab)
  "Create the key for TAB.

This uses a message digest of the tab, a random number, the Emacs
PID and `recent-keys' vector."
  (let ((object (format "%s%s%s%s" tab (random) (emacs-pid) (recent-keys))))

    (md5 object)))

(defun partial-recall--ring-oldest (ring)
  "Get the oldest element in RING."
  (unless (ring-empty-p ring)
    (ring-ref ring (1- (ring-length ring)))))

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
  (let ((mapped (partial-recall--mapped include-subconscious)))

    (mapcar #'partial-recall--moment-buffer mapped)))

(defun partial-recall--mapped (&optional include-subconscious)
  "Get all moments.

This excludes moments in the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (cl-loop for k being the hash-keys of partial-recall--table
           using (hash-values memory)
           unless (and (not include-subconscious)
                       (string= k partial-recall--subconscious-key))
           append (ring-elements (partial-recall--memory-ring memory))))

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

(defun partial-recall--focus (&optional buffer)
  "Get the focus of BUFFER."
  (when-let* ((buffer (or buffer (current-buffer)))
              (moment (partial-recall--moment-from-buffer buffer)))

    (partial-recall--moment-focus moment)))

(defun partial-recall--name (memory)
  "Get the name of MEMORY."
  (if (partial-recall--subconsciousp memory)
      partial-recall--subconscious-key
    (partial-recall--tab-name-all-frames memory)))

(defun partial-recall--tab (memory &optional frame)
  "Get the tab for MEMORY.

Optionally search in FRAME."
  (when-let ((key (partial-recall--memory-key memory))
             (tabs (if frame
                       (with-selected-frame frame
                         (funcall tab-bar-tabs-function frame))
                     (funcall tab-bar-tabs-function))))

    (seq-find (lambda (it) (string= key (alist-get 'pr it))) tabs)))

(defun partial-recall--tab-name-all-frames (memory)
  "Get the tab name for MEMORY searching all frames."
  (let ((frames (frame-list)))
    (catch 'found
      (dotimes (ind (length frames))
        (when-let* ((frame (nth ind frames))
                    (name (partial-recall--tab-name memory frame)))

          (throw 'found name))))))

(defun partial-recall--tab-name (&optional memory frame)
  "Get the tab name for MEMORY.

Optionally search in FRAME."
  (when-let ((memory (or memory (partial-recall--reality)))
             (tab (partial-recall--tab memory frame)))
    (alist-get 'name tab)))

(defun partial-recall--memories (&optional exclude-subconscious)
  "Get all memories.

If EXCLUDE-SUBCONSCIOUS is t, it is excluded."
  (let ((memories (hash-table-values partial-recall--table)))

    (if exclude-subconscious
        (seq-filter (lambda (it) (not (partial-recall--subconsciousp it))) memories)
      memories)))

(defun partial-recall--memory-by-key (key)
  "Get or create memory identified by KEY."
  (when key
    (if-let* ((table partial-recall--table)
              (memory (gethash key table)))

        memory

      (let ((new-memory (partial-recall--memory-create key)))

        (puthash key new-memory table)
        new-memory))))

(defun partial-recall--reality ()
  "Get the current memory."
  (partial-recall--memory-by-key (partial-recall--key)))

(defun partial-recall--subconscious ()
  "Return (or create) the subconscious."
  (partial-recall--memory-by-key partial-recall--subconscious-key))

(defun partial-recall--moments-member (moments buffer)
  "Check if BUFFER is a member of MOMENTS."
  (catch 'found
    (dotimes (ind (ring-length moments))
      (when (equal buffer (partial-recall--moment-buffer (ring-ref moments ind)))
        (throw 'found ind)))))

(defun partial-recall--moment-set-permanence (moment permanence)
  "Set MOMENT PERMANENCE."
  (setf (partial-recall--moment-permanence moment) permanence)

  (with-current-buffer (partial-recall--moment-buffer moment)
    (setq-local partial-recall--implanted permanence))

  (run-hooks 'partial-recall-permanence-change-hook)

  moment)

(defun partial-recall--moment-update-timestamp (moment)
  "Update the timestamp for MOMENT."
  (setf (partial-recall--moment-timestamp moment) (floor (time-to-seconds)))

  moment)

(defun partial-recall--increase-focus (moment amount)
  "Increment the focus for MOMENT by AMOUNT.

Permanent moments do not gain additional focus."
  (and-let* (((not (partial-recall--moment-permanence moment)))
             (count (partial-recall--moment-focus moment))
             (updated-count (+ count amount)))

    (partial-recall--maybe-implant-moment moment updated-count)

    (setf (partial-recall--moment-focus moment) updated-count)

    moment))

(defun partial-recall--intensify (moment &optional reset context)
  "Intensify MOMENT.

This will update its timestamp and increment its focus.

If CONTEXT has a value in `partial-recall-intensities', increase
by that amount.

If RESET is t, reset the focus instead and remove permanence."
  (when reset
      (partial-recall--reset-count moment)
      (partial-recall--moment-set-permanence moment nil))

  (let ((contextual (alist-get context partial-recall-intensities)))

    (partial-recall--increase-focus moment (or contextual 0)))

  (partial-recall--moment-update-timestamp moment))

(defun partial-recall--reset-count (moment)
  "Reset the focus for MOMENT."
  (setf (partial-recall--moment-focus moment) 0)

  moment)

(defun partial-recall--remove-buffer (buffer memory)
  "Remove BUFFER from MEMORY and return it."
  (when-let* ((moments (partial-recall--memory-ring memory))
              (index (partial-recall--moments-member moments buffer))
              (removed (ring-remove moments index)))

    removed))

(defun partial-recall--insert (ring item &optional extend)
  "Insert ITEM in RING.

If EXTEND is t, also extend."
  (ring-insert+extend ring item extend)

  (run-hooks 'partial-recall-after-insert-hook)

  item)

;;; -- Dealing with frames

(defun partial-recall--from-other-frame (memory)
  "Get tab and frame of MEMORY."
  (when-let* ((other-frames (filtered-frame-list #'partial-recall--is-other-frame-p))
              (found (catch 'found
                       (dotimes (ind (length other-frames))
                         (when-let* ((frame (nth ind other-frames))
                                     (tab (partial-recall--tab memory frame)))

                           (throw 'found (list :tab tab :frame frame)))))))
    found))

(defmacro partial-recall--in-other-frame (memory &rest forms)
  "Evaluate FORMS in other frame.

If MEMORY is not in another form, this is a no-op."
  (declare (indent defun))

  `(and-let* ((partial-recall--foreignp ,memory)
              (info (partial-recall--from-other-frame ,memory))
              (frame (plist-get info :frame)))

     (partial-recall--debug "Evaluating on behalf of %s in other frame" ,memory)

     (with-selected-frame frame
       ,@forms)))

;;; -- Handlers

(defun partial-recall--schedule-buffer (buffer)
  "Schedule handling BUFFER."
  (with-current-buffer buffer
    (and-let* ((buffer (current-buffer))
               ((partial-recall--new-buffer-p buffer))
               ((partial-recall--meaningful-buffer-p buffer)))

      (partial-recall--void-timer)

      (partial-recall--debug "Scheduling buffer '%s'" buffer)

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
  (partial-recall--void-timer)

  (when (partial-recall--buffer-visible-p buffer)

    (partial-recall--log "Handling buffer '%s'" buffer)

    (if (partial-recall--mapped-buffer-p buffer)
        (partial-recall--recollect buffer)
      (partial-recall--remember buffer))

    (setq partial-recall--last-checked buffer)))

(defun partial-recall--void-timer ()
  "Void the current timer."
  (when partial-recall--timer
    (unless (timer--triggered partial-recall--timer)
      (partial-recall--debug "Canceling previous timer '%s'" partial-recall--timer)
      (cancel-timer partial-recall--timer))

    (setq partial-recall--timer nil)))

(defun partial-recall--concentrate ()
  "Concentrate on the current moment.

If the moment has remained the same since the last cycle, its
focus is intensified, otherwise concentration breaks."
  (if (partial-recall--can-hold-concentration-p)
      (progn
        (partial-recall--debug "Concentration held on '%s'" partial-recall--last-focus)
        (partial-recall--intensify partial-recall--last-focus nil 'concentrate))

    (when partial-recall--last-focus
      (partial-recall--debug "Concentration on '%s' broke" partial-recall--last-focus))

    (setq partial-recall--last-focus (partial-recall--moment-from-buffer (current-buffer)))))

;;; -- Reactions

(defun partial-recall--before-switch-to-buffer (buffer &optional norecord &rest _)
  "Maybe switch memories before scheduling BUFFER.

Don't do anything if NORECORD is t."
  (unless norecord
    (partial-recall--maybe-switch-memory (get-buffer buffer))))

(defun partial-recall--after-switch-to-buffer (buffer &optional norecord &rest _)
  "Schedule the BUFFER that was switched to.

Don't do anything if NORECORD is t."
  (unless norecord
    (partial-recall--schedule-buffer buffer)))

(defun partial-recall--after-pop-to-buffer (buffer &optional _action norecord)
  "Schedule the BUFFER that was popped to.

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
      (partial-recall--clean-up-buffer (partial-recall--moment-buffer it))
      (partial-recall--suppress it))

    (remhash tab-key table)))

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

(defun partial-recall--after-register-val-jump-to (value &rest _args)
  "Maybe switch memory after jumping to VALUE."
  (cond
   ((window-configuration-p (car-safe value))
    (when-let* ((marker (cadr value))
                (buffer (marker-buffer marker)))

      (partial-recall--maybe-switch-memory buffer t)))))

(defun partial-recall--after-winner (&rest _)
  "Maybe switch memory after `winner-undo' or `winner-redo'."
  (when-let* ((foreign (seq-find
                        (lambda (it)
                          (not (partial-recall--memory-buffer-p (window-buffer it))))
                        (window-list))))

    (partial-recall--maybe-switch-memory (window-buffer foreign) t)))

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

    (unless (partial-recall--lift buffer)
      (let ((moment (partial-recall--moment-create buffer)))

        (partial-recall--insert ring moment)))))

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
  (if-let* ((reality (partial-recall--reality))
            (owner (partial-recall--buffer-owner buffer))
            ((not (eq reality owner)))
            (moment (partial-recall--moment-from-buffer buffer owner))
            ((or force
                 (and
                  (not (partial-recall--moment-permanence moment))
                  (partial-recall--exceedsp moment partial-recall-reclaim-min-age)))))

      (progn
        (partial-recall--in-other-frame owner
          (partial-recall--clean-up-window buffer))

        (partial-recall--swap owner reality moment))
    (partial-recall--debug "Won't claim `%s'" buffer)))

(defun partial-recall--forget (&optional buffer suppress)
  "Forget BUFFER.

This will remove the buffer's moment from the memory. If SUPPRESS
is t, the forgotten moment goes into the subconscious."
  (when-let* ((buffer (or buffer (current-buffer)))
              ((partial-recall--mapped-buffer-p buffer t))
              (maybe-remove (lambda (key memory)
                              (when-let ((moment (partial-recall--remove-buffer buffer memory)))

                                (when (and suppress
                                           (not (string= key partial-recall--subconscious-key)))
                                  (partial-recall--suppress moment))

                                (partial-recall--probe-memory memory)

                                (throw 'found memory)))))

    (partial-recall--clean-up-buffer buffer)

    (let ((memory (catch 'found
                    (maphash maybe-remove partial-recall--table))))

      (partial-recall--log "'%s' was removed from '%s'" buffer memory))))

(defun partial-recall--forget-some ()
  "Prompt the user to forget some moments."
  (let ((moments (partial-recall--mapped)))

    (while moments
      (and-let* ((moment (car moments))
                 (buffer (partial-recall--moment-buffer moment))
                 (name (buffer-name buffer))
                 ((not (string-equal name ""))))


        (when (yes-or-no-p (format "Forget %s (%s)?"
                                   (partial-recall--repr moment)
                                   (if (buffer-modified-p buffer)
                                       "modified"
                                     "unmodified")))

          (partial-recall--forget buffer t))

        (setq moments (cdr moments))))))

(defun partial-recall--reject (buffer memory)
  "Reject BUFFER and push it to MEMORY."
  (let ((reality (partial-recall--reality)))

    (when (equal memory reality)
      (user-error "The current reality can't be the target of the rejected buffer"))

    (when (not (equal (partial-recall--buffer-owner buffer)
                      reality))
      (user-error "The buffer to reject does not belong to the current reality"))

    (when-let ((moment (partial-recall--moment-from-buffer buffer)))

      (partial-recall--swap reality memory moment)
      (partial-recall--clean-up-buffer buffer))))

(defun partial-recall--implant (&optional buffer excise)
  "Make BUFFER's moment permanent.

A permanent moment can not be reclaimed and will not be
automatically forgotten.

If EXCISE is t, remove permanence instead."
  (when-let* ((buffer (or buffer (current-buffer)))
              (moment (partial-recall--moment-from-buffer buffer))
              (verb (if excise "Excising" "Implanting")))

    (unless (eq (partial-recall--moment-permanence moment) (not excise))

      (partial-recall--log "%s '%s'" verb moment)

      (partial-recall--moment-set-permanence moment (not excise))

      (when excise
        (partial-recall--reset-count moment)))))

(defun partial-recall--suppress (moment)
  "Suppress MOMENT in the subconscious."
  (and-let* ((buffer (partial-recall--moment-buffer moment))
             (memory (partial-recall--subconscious))
             ((not (partial-recall--memory-buffer-p buffer memory)))
             ((partial-recall--meaningful-buffer-p buffer))
             (ring (partial-recall--memory-ring memory)))

    (when (partial-recall--memory-at-capacity-p memory)
      (let* ((removed (ring-remove ring))
             (buffer (partial-recall--moment-buffer removed)))

        (when partial-recall-repress
          (partial-recall--log "Repressing '%s'" removed)
          (kill-buffer buffer))))

    (partial-recall--debug "Suppressing '%s'" moment)

    (partial-recall--intensify moment t 'suppress)

    (partial-recall--insert ring moment)))

(defun partial-recall--lift (buffer)
  "Lift BUFFER into reality."
  (when-let* ((sub (partial-recall--subconscious))
              (reality (partial-recall--reality))
              (moment (partial-recall--moment-from-buffer buffer)))

    (partial-recall--log "Lifting '%s' out of the subconscious" moment)

    (partial-recall--swap sub reality moment t)

    moment))

(defun partial-recall--recollect (buffer)
  "Recollect the BUFFER.

Recollection happens to mapped buffers. Those belonging to the
current reality are reinforced. Those of other memories
are (potentially) reclaimed."
  (if (partial-recall--memory-buffer-p buffer)
      (partial-recall--reinforce buffer)
    (when partial-recall-reclaim
      (partial-recall--reclaim buffer))))

(defun partial-recall--swap (a b moment &optional reset)
  "Swap MOMENT from memory A to B.

Both memories will be probed. Memory A after the moment was
removed, memory B before it is inserted.

If RESET is t, reset the swapped moment."
  (and-let* ((a-ring (partial-recall--memory-ring a))
             (b-ring (partial-recall--memory-ring b))
             (index (ring-member a-ring moment))
             (removed (ring-remove a-ring index)))

    (partial-recall--log "Swapping '%s' from '%s' to '%s'" moment a b)

    (partial-recall--probe-memory a)
    (partial-recall--probe-memory b)

    (partial-recall--intensify moment reset 'swap)

    (partial-recall--insert b-ring removed)))

(defun partial-recall--reinsert (moment memory)
  "Reinsert MOMENT into MEMORY.

This removes, inserts and extends. The moment is refreshed."
  (and-let* ((ring (partial-recall--memory-ring memory))
             ((ring-member ring moment))
             (buffer (partial-recall--moment-buffer moment)))

    (partial-recall--debug "Re-inserting '%s' in '%s'" moment memory)

    (partial-recall--intensify moment nil 'reinsert)

    (ring-remove+insert+extend ring moment t)))

(defun partial-recall--meld (a b &optional close)
  "Meld memories A and B.

This moves all of the moments in A to B.

If CLOSE is t, the tab of B is closed."
  (when (eq a b)
    (user-error "Can't shift moments from identical memories"))

  (let ((moments-a (partial-recall--memory-ring a))
        (moments-b (partial-recall--memory-ring b)))

    (while (not (ring-empty-p moments-a))
      (let ((moment (ring-remove moments-a)))
        (partial-recall--insert moments-b moment t)))

    (when close
      (tab-bar-close-tab-by-name (partial-recall--tab-name a)))))

(defun partial-recall--flush (memory &optional arg)
  "Flush MEMORY.

If MEMORY is not provided, flush the reality.

This will call all functions of `partial-recall-memorable-traits'
to check if the moment should be kept, passing moment and ARG."
  (let* ((ring (partial-recall--memory-ring memory))
         (count 0))

    (dolist (moment (ring-elements ring))
      (unless (seq-some (lambda (it) (funcall it moment arg)) partial-recall-memorable-traits)

        (setq count (1+ count))

        (let* ((index (ring-member ring moment))
               (removed (ring-remove ring index)))

          (partial-recall--suppress removed))

        (partial-recall--clean-up-buffer (partial-recall--moment-buffer moment))))

    (partial-recall--log "Flushed %d moments from '%s'" count memory)

    (partial-recall--probe-memory memory)))

(defun partial-recall--clean-up-window (buffer &optional frame)
  "Clean up BUFFER's window.

Optionally in FRAME."
  (when-let* ((windows (window-list frame)))
    (dolist (window windows)
      (when (eq (window-buffer window) buffer)
        (quit-window nil window)))))

(defun partial-recall--clean-up-buffer (buffer)
  "Clean up BUFFER if necessary.

Deletes any window currently displaying it and makes sure it is
no longer recorded as the last checked buffer."
  (dolist (frame (frame-list))
    (partial-recall--clean-up-window buffer frame)

    (when (eq partial-recall--last-checked buffer)
      (setq partial-recall--last-checked nil))

    (when (and partial-recall--last-focus
               (eq (partial-recall--moment-buffer partial-recall--last-focus)
                   buffer))
      (setq partial-recall--last-focus nil))))

(defun partial-recall--probe-memory (memory)
  "Probe MEMORY.

This will reinsert implanted moments, suppress removed moments,
as well as resize and extend the memory if necessary."
  (or  (partial-recall--maybe-resize-memory memory)
       (partial-recall--maybe-extend-memory memory))
  (partial-recall--maybe-reinsert-implanted memory)
  (partial-recall--maybe-suppress-oldest-moment memory)

  (run-hooks 'partial-recall-probe-hook))

(defun partial-recall--maybe-reinsert-implanted (memory)
  "Maybe reinforce oldest moments in MEMORY.

This will loop over the moments in reverse and makes sure to
re-insert any implanted one."
  (and-let* ((ring (partial-recall--memory-ring memory))
             (oldest (partial-recall--ring-oldest ring)))

    (let ((checked nil))

      (while (and oldest
                  (not (memq oldest checked))
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
      (partial-recall--debug "Resizing '%s'" memory)

      (ring-resize ring (max (ring-length ring) orig)))))

(defun partial-recall--maybe-extend-memory (memory)
  "Maybe extend MEMORY.

See `partial-recall--should-extend-memory-p'."
  (when (and (partial-recall--memory-at-capacity-p memory)
             (partial-recall--should-extend-memory-p memory))
    (partial-recall--debug "Extending '%s'" memory)

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

This is true if COUNT exceeds `partial-recall-auto-implant'."
  (when (and (numberp partial-recall-auto-implant)
             (not (partial-recall--moment-permanence moment))
             (> count partial-recall-auto-implant))

    (partial-recall--implant (partial-recall--moment-buffer moment))))

(defun partial-recall--maybe-switch-memory (&optional buffer unscheduled)
  "Maybe switch to BUFFER's memory.

Memories in the subconscious are not considered.

If UNSCHEDULED is t don't account for reclaiming."
  (and-let* (partial-recall-auto-switch
             (buffer (or buffer (current-buffer)))
             ((partial-recall--mapped-buffer-p buffer))
             ((not (partial-recall--memory-buffer-p buffer)))
             (moment (partial-recall--moment-from-buffer buffer))
             ((or unscheduled
                  (not (partial-recall--exceedsp moment (- partial-recall-reclaim-min-age
                                                            partial-recall-handle-delay)))))
             (owner (partial-recall--buffer-owner buffer))
             ((pcase partial-recall-auto-switch
                ('prompt
                 (yes-or-no-p (format "Switch to %s?" (partial-recall--repr owner))))
                ('t t)
                (_ nil))))

    (with-current-buffer buffer
      (tab-bar-switch-to-tab (partial-recall--tab-name owner)))))

(defun partial-recall--switch-to-and-neglect (buffer)
  "Switch to BUFFER and make sure it is neglected.

This means the buffer won't be scheduled for handling."
  (setq partial-recall--neglect buffer)
  (funcall partial-recall--switch-to-buffer-function buffer))

(defun partial-recall--previous-buffer ()
  "Get the previous moment."
  (when-let* ((memory (partial-recall--reality))
              (current (partial-recall--moment-from-buffer (current-buffer) memory))
              (previous (ring-previous (partial-recall--memory-ring memory) current)))

    (partial-recall--moment-buffer previous)))

(defun partial-recall--next-buffer ()
  "Get the next moment."
  (when-let* ((memory (partial-recall--reality))
              (current (partial-recall--moment-from-buffer (current-buffer) memory))
              (next (ring-next (partial-recall--memory-ring memory) current)))

    (partial-recall--moment-buffer next)))

;;; -- Traits

(defun partial-recall--meaningful-buffer-p (buffer)
  "Check if BUFFER should be remembered."
  (let ((verify (apply-partially #'partial-recall--maybe-call-with-buffer buffer)))

    (seq-every-p verify partial-recall-meaningful-traits)))

(defun partial-recall--not-filtered-p (buffer)
  "Verify that BUFFER isn't filtered."
  (let ((filter (mapconcat (lambda (it) (concat "\\(?:" it "\\)")) partial-recall-filter "\\|")))

    (not (string-match-p filter (buffer-name buffer)))))

(defun partial-recall--not-in-view-mode-p (buffer)
  "Make sure BUFFER is not in `view-mode'."
  (not (buffer-local-value 'view-mode buffer)))

(defun partial-recall--gracedp (moment &optional arg)
  "Check if MOMENT was graced.

This is either a permanent moment, a moment that has been focused
once or a moment that couldn't be reclaimed.

If ARG is t, the current moment is considered graced as well."
  (or (partial-recall--moment-permanence moment)
      (> 0 (partial-recall--moment-focus moment))
      (partial-recall--falls-below-p moment partial-recall-reclaim-min-age)
      (and arg
           (eq (current-buffer) (partial-recall--moment-buffer moment)))))

(put 'buffer-file-name
     'partial-recall-non-meaningful-explainer
     "Buffer not associated with a file")
(put 'partial-recall--not-filtered-p
     'partial-recall-non-meaningful-explainer
     "Buffer is filtered by `partial-recall-filter'")
(put 'partial-recall--not-in-view-mode-p
     'partial-recall-non-meaningful-explainer
     "Buffer is in `view-mode'")

;;; -- Conditionals

(defun partial-recall--buffer-visible-p (buffer)
  "Check that BUFFER remains visible.

This also checks for buffers that might have been obscured."
  (let* ((windows (partial-recall--window-list))
         (visible (and buffer
                       (buffer-live-p buffer)
                       (or (eq buffer partial-recall--before-minibuffer)
                           (memq buffer (mapcar #'window-buffer windows)))))
         (verb (if visible "remains" "is no longer")))

    (partial-recall--debug "Buffer '%s' %s visible" buffer verb)

    visible))

(defun partial-recall--memory-at-capacity-p (memory)
  "Check if MEMORY is at capacity."
  (when-let ((ring (partial-recall--memory-ring memory)))

    (= (ring-length ring) (ring-size ring))))

(defun partial-recall--memory-buffer-p (buffer &optional memory)
  "Check if BUFFER is a member of MEMORY.

If MEMORY is not passed, use the current reality."
  (partial-recall--moments-member
   (partial-recall--memory-ring (or memory (partial-recall--reality)))
   buffer))

(defun partial-recall--moment-buffer-p (buffer moment)
  "Check if BUFFER is encapsulated by MOMENT."
  (eq (partial-recall--moment-buffer moment) buffer))

(defun partial-recall--mapped-buffer-p (buffer &optional include-subconscious)
  "Check if BUFFER is mapped.

This excludes moments in the subconscious unless
INCLUDE-SUBCONSCIOUS is t."
  (let ((buffers (partial-recall--mapped-buffers include-subconscious)))

    (memq buffer buffers)))

(defun partial-recall--realityp (memory)
  "Check if MEMORY is the reality."
  (eq (partial-recall--reality) memory))

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

(defun partial-recall--subconsciousp (memory)
  "Check if MEMORY is the subconscious."
  (string= partial-recall--subconscious-key (partial-recall--memory-key memory)))

(defun partial-recall--exceedsp (moment threshold)
  "Check if MOMENT's age exceeds THRESHOLD."
  (< threshold
     (- (floor (time-to-seconds))
        (partial-recall--moment-timestamp moment))))

(defun partial-recall--falls-below-p (moment threshold)
  "Check if MOMENT's age falls below THRESHOLD."
  (> threshold
     (- (floor (time-to-seconds))
        (partial-recall--moment-timestamp moment))))

(defun partial-recall--new-buffer-p (buffer)
  "Check if BUFFER is actually new."
  (cond
   ((eq partial-recall--last-checked buffer)
    nil)
   ((eq partial-recall--neglect buffer)
    (setq partial-recall--neglect nil))
   (t t)))

(defun partial-recall--can-hold-concentration-p ()
  "Check if concentration can be held."
  (and partial-recall--last-focus
       (or (eq (partial-recall--moment-from-buffer (current-buffer))
               partial-recall--last-focus)
           (partial-recall--buffer-visible-p (partial-recall--moment-buffer partial-recall--last-focus)))))

(defun partial-recall--is-other-frame-p (frame)
  "Check that FRAME is not the selected frame."
  (not (eq frame (selected-frame))))

(defun partial-recall--foreignp (memory)
  "Check if MEMORY belongs to foreign frame."
  (null (partial-recall--tab memory)))

;;; -- Utility

(defun partial-recall--window-list ()
  "Get all windows."
  (cl-loop for frame in (visible-frame-list)
           append (window-list frame)))

(defun partial-recall--maybe-call-with-buffer (buffer fun)
  "Call FUN maybe with BUFFER."
  (let* ((arity (func-arity fun))
         (min (car arity))
         (max (cdr arity)))
    (if (or (> min 0)
            (eq 'many max)
            (and (numberp max)
                 (> max 0)))
        (funcall fun buffer)
      (partial-recall--warn "Function '%s' has the wrong arity" fun)
      t)))

(defun partial-recall--explain-omission (&optional buffer)
  "Get the reason why BUFFER is omitted.

This will try to find the first trait in
`partial-recall-meaningful-traits' that returns falsy and return
its explainer (property
`partial-recall-non-meaningful-explainer')if it exists."
  (and-let* ((buffer (or buffer (current-buffer)))
             (verify (apply-partially 'partial-recall--maybe-call-with-buffer buffer))
             (finder (lambda (it) (not (funcall verify it))))
             (failing (seq-find finder partial-recall-meaningful-traits))
             (explainer (get failing 'partial-recall-non-meaningful-explainer)))

    explainer))

(defvar partial-recall-graph--blocks ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"])
(defvar partial-recall-graph--ratios '(0.125 0.25 0.375 0.5 0.625 0.75 0.875 1))

(defun partial-recall-graph (val max)
  "Graph VAL.

Selects a symbol based on VAL's relation to MAX."
  (let ((index 0)
        (max-index (1- (length partial-recall-graph--blocks))))

    (unless (zerop val)
      (while (and (> val (* max (nth index partial-recall-graph--ratios)))
                  (< index max-index))
        (setq index (1+ index)))

      (aref partial-recall-graph--blocks index))))

;;; -- Printing

(defun partial-recall--warn (message &rest args)
  "Warn about MESSAGE.

Message will be formatted with ARGS."
  (display-warning 'partial-recall (apply #'format message args) :warning))

(defun partial-recall--log (fmt &rest args)
  "Use ARGS to format FMT if not silenced."
  (when partial-recall-log
    (let* ((fmt (partial-recall--prefix-fmt-string fmt))
           (args (mapcar #'partial-recall--repr args)))

      (apply 'message fmt args))))

(defun partial-recall--debug (fmt &rest args)
  "Use ARGS to format FMT if debug is enabled."
  (when (and (numberp partial-recall-log)
             (< partial-recall-log 1))
    (apply 'partial-recall--log fmt args)))

(defun partial-recall--message (fmt &rest args)
  "Use ARGS to format FMT and always show."
  (let ((partial-recall-log t))

    (apply 'partial-recall--log fmt args)))

(defun partial-recall--prefix-fmt-string (format-string)
  "Prefix FORMAT-STRING."
  (if partial-recall-log-prefix
      (concat (propertize partial-recall-log-prefix 'face 'partial-recall-emphasis) " :: " format-string)
    format-string))

(defun partial-recall--repr (thing)
  "Format THING if it's a custom structure."
  (pcase (type-of thing)

    ('partial-recall--moment
     (let ((buffer (partial-recall--moment-buffer thing))
           (ts (partial-recall--moment-timestamp thing)))

       (format
        "#<moment %s (%s)>"
        (buffer-name buffer)
        (format-time-string "%H:%M:%S" (seconds-to-time ts)))))

    ('partial-recall--memory
     (let ((ring (partial-recall--memory-ring thing))
           (name (partial-recall--name thing)))

       (format
        "#<memory %s (%d/%d)>"
        name
        (ring-length ring)
        (ring-size ring))))
    (_ thing)))

;;; -- Completion

(defun partial-recall--complete-dream (prompt)
  "Complete dream buffer using PROMPT."
  (let* ((predicate (lambda (it) (and-let* ((buffer (cdr it))
                                       ((partial-recall--mapped-buffer-p buffer))
                                       ((not (partial-recall--memory-buffer-p buffer)))))))
         (current (current-buffer))
         (initial (unless (partial-recall--memory-buffer-p current)
                    (buffer-name current))))

    (partial-recall--complete-buffer prompt predicate initial)))

(defun partial-recall--complete-reality (prompt &optional no-preselect exclude-current)
  "Complete reality buffer using PROMPT.

If NO-PRESELECT is t, no initial input is set.

If EXCLUDE-CURRENT is t, don't include the current buffer."
  (let* ((predicate (lambda (it) (partial-recall--memory-buffer-p (cdr it))))
         (current (current-buffer))
         (initial (when (and (not no-preselect) (partial-recall--memory-buffer-p current))
                    (buffer-name current))))

    (partial-recall--complete-buffer prompt predicate initial exclude-current)))

(defun partial-recall--complete-subconscious (prompt)
  "Complete subconscious buffer using PROMPT."
  (let* ((memory (partial-recall--subconscious))
         (predicate (lambda (it) (partial-recall--memory-buffer-p (cdr it) memory)))
         (current (current-buffer))
         (initial (when (partial-recall--memory-buffer-p current memory)
                    (buffer-name current))))

    (partial-recall--complete-buffer prompt predicate initial)))

(defun partial-recall--complete-any (prompt &optional allow-meaningless)
  "Complete any buffer using PROMPT.

Mapped buffers and non-file buffers (unless ALLOW-MEANINGLESS is t)
are not considered."
  (let* ((predicate (if allow-meaningless
                        #'always
                      (lambda (it) (partial-recall--meaningful-buffer-p (cdr it)))))
         (current (current-buffer))
         (initial (unless (or (partial-recall--mapped-buffer-p current t)
                              (and (not (partial-recall--meaningful-buffer-p current))
                                   (not allow-meaningless)))
                    (buffer-name current))))

    (partial-recall--complete-buffer prompt predicate initial)))

(defun partial-recall--complete-buffer (prompt predicate &optional initial exclude-current)
  "Complete BUFFERS with PREDICATE.

Optionally provide INITIAL input.

Completion is done using `read-buffer' with PROMPT after setting
up completion table.

If EXCLUDE-CURRENT is t, don't include the current buffer."
  (let* ((rbts-completion-table (if exclude-current
                                    (internal-complete-buffer-except)
                                  (apply-partially
                                   #'completion-table-with-predicate
                                   #'internal-complete-buffer
                                   #'always
                                   nil))))

    (minibuffer-with-setup-hook
        (lambda () (setq-local minibuffer-completion-table rbts-completion-table))

      (get-buffer (read-buffer prompt initial t predicate)))))

(defun partial-recall--complete-memory (prompt &optional include-subconscious)
  "Complete memory using PROMPT.

The subconscious is not included unless INCLUDE-SUBCONSCIOUS is
t."
  (let* ((memories (partial-recall--memories (not include-subconscious)))
         (collection (mapcar (lambda (it) (cons (partial-recall--name it) it)) memories))
         (selection (completing-read prompt collection nil t)))

    (cdr-safe (assoc selection collection))))

;;; -- Lighter

(defvar partial-recall--lighter-map
  (let ((map (make-sparse-keymap)))

    (define-key map [mode-line mouse-1] 'partial-recall--lighter-toggle)
    (define-key map [mode-line mouse-3] 'partial-recall--lighter-menu)

    map))

(defvar partial-recall--lighter-title
  `(:propertize partial-recall-lighter-prefix
                face partial-recall-deemphasized
                mouse-face mode-line-highlight
                help-echo "Partial Recall\nmouse-1: Implant/Excise\nmouse-3: Menu"
                local-map ,partial-recall--lighter-map))

(defun partial-recall--lighter-title ()
  "Show the title.

The title has a menu."
  partial-recall--lighter-title)

(defun partial-recall--lighter-toggle ()
  "Implant or excise the current buffer."
  (interactive)

  (when-let* ((buffer (current-buffer))
              (moment (partial-recall--moment-from-buffer buffer)))

    (partial-recall-implant (current-buffer) (partial-recall--moment-permanence moment))))

(defun partial-recall--lighter-menu ()
  "Show a menu.."
  (interactive)

  (let* ((map (make-sparse-keymap))
         (rename (lambda (sym) (substring (symbol-name sym)
                                     (1+ (length "partial-recall")))))
         (bind (lambda (_event func)
                 (define-key-after map
                   (vector func)
                   (list 'menu-item (funcall rename func) func)))))

    (define-key-after map [--actions] (list 'menu-item "Partial Recall"))

    (map-keymap bind partial-recall-command-map)

    (condition-case nil
        (popup-menu map)
      (quit nil))))

(defun partial-recall--lighter-moment ()
  "Show moment information.

This will show a propertized asterisk if the moment is permanent."
  (if-let* ((buffer (current-buffer))
            (moment (partial-recall--moment-from-buffer buffer)))

      (if (partial-recall--moment-permanence moment)
          '(:propertize "*"
                        face partial-recall-contrast
                        help-echo "Moment is implanted")
        '(:propertize "-"
                      face partial-recall-deemphasized
                      help-echo "Moment is fleeting"))
    (if (partial-recall--meaningful-buffer-p buffer)
        '(:propertize "?"
                      face partial-recall-deemphasized
                      help-echo "Considering")
      (let* ((explanation (partial-recall--explain-omission))
             (face (if explanation 'partial-recall-emphasis 'partial-recall-deemphasized))
             (echo (if explanation (format "Not meaningful: %s" explanation) "Not meaningful")))
        `(:propertize "!"
                      face ,face
                      help-echo ,echo)))))

(defun partial-recall--lighter-memory ()
  "Show memory information.

This will normally show the current number of items in the
memory; if the memory has exceeded its original size, the surplus
is shown."
  (and-let* ((memory (partial-recall--reality))
             (ring (partial-recall--memory-ring memory))
             (orig-size (partial-recall--memory-orig-size memory))
             (size (ring-size ring))
             (length (ring-length ring)))

    (if (> size orig-size)
        `(:propertize "+"
                      face partial-recall-emphasis
                      help-echo ,(format "Memory has grown to +%d" (- size orig-size)))
      `(:propertize ,(or (partial-recall-graph length size) "-")
                    face partial-recall-deemphasized
                    help-echo ,(format "Memory contains %d moment(s)" length)))))

(defvar partial-recall-lighter-title '(:eval (partial-recall--lighter-title)))
(defvar partial-recall-lighter-moment '(:eval (partial-recall--lighter-moment)))
(defvar partial-recall-lighter-memory '(:eval (partial-recall--lighter-memory)))

;; If the variables are not considered risky, the mode line constructs
;; they contain are not evaluated.
(put 'partial-recall-lighter 'risky-local-variable t)
(put 'partial-recall-lighter-title 'risky-local-variable t)
(put 'partial-recall-lighter-moment 'risky-local-variable t)
(put 'partial-recall-lighter-memory 'risky-local-variable t)

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

  (setq partial-recall--concentration-timer (run-with-timer
                                             1
                                             partial-recall--concentration-repeat
                                             #'partial-recall--concentrate))

  (advice-add
   partial-recall--switch-to-buffer-function :before
   #'partial-recall--before-switch-to-buffer)
  (advice-add
   partial-recall--switch-to-buffer-function :after
   #'partial-recall--after-switch-to-buffer)
  (advice-add
   partial-recall--pop-to-buffer-function :after
   #'partial-recall--after-pop-to-buffer)
  (advice-add
   'register-val-jump-to :before
   #'partial-recall--after-register-val-jump-to)
  (advice-add
   'winner-undo :after
   #'partial-recall--after-winner)
  (advice-add
   'winner-redo :after
   #'partial-recall--after-winner)

  (add-hook 'minibuffer-setup-hook #'partial-recall--on-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'partial-recall--on-minibuffer-exit)
  (add-hook 'after-make-frame-functions #'partial-recall--queue-fix-up)
  (add-hook 'kill-buffer-hook #'partial-recall--forget)
  (add-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (add-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)
  (add-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

(defun partial-recall-mode--teardown ()
  "Tear down `partial-recall-mode'."
  (cancel-timer partial-recall--concentration-timer)

  (advice-remove
   partial-recall--switch-to-buffer-function
   #'partial-recall--before-switch-to-buffer)
  (advice-remove
   partial-recall--switch-to-buffer-function
   #'partial-recall--after-switch-to-buffer)
  (advice-remove
   partial-recall--pop-to-buffer-function
   #'partial-recall--after-pop-to-buffer)
  (advice-remove
   'register-val-jump-to
   #'partial-recall--after-register-val-jump-to)
  (advice-remove
   'winner-undo
   #'partial-recall--after-winner)
  (advice-remove
   'winner-redo
   #'partial-recall--after-winner)

  (remove-hook 'minibuffer-setup-hook #'partial-recall--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'partial-recall--on-minibuffer-exit)
  (remove-hook 'after-make-frame-functions #'partial-recall--queue-fix-up)
  (remove-hook 'kill-buffer-hook #'partial-recall--forget)
  (remove-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (remove-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)
  (remove-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

;;; -- API

;;;###autoload
(define-minor-mode partial-recall-mode
  "Keep track of buffers opened in a tab."
  :lighter partial-recall-lighter
  :global t
  (if partial-recall-mode
      (partial-recall-mode--setup)
    (partial-recall-mode--teardown)))

;;;###autoload
(defun partial-recall-remember (buffer)
  "Remember BUFFER.

This will add the buffer to the current memory and subsequently
switch to it.

The selection is limited to buffers that do not yet belong to a
memory. If called with a prefix argument, the selection will be
widened to all buffers."
  (interactive (list (partial-recall--complete-any "Remember buffer: " current-prefix-arg)))

  (partial-recall--remember buffer)

  (partial-recall--switch-to-and-neglect buffer))

;;;###autoload
(defun partial-recall-switch-to-buffer (buffer &optional arg)
  "Switch to BUFFER.

The selection is limited to buffers belonging to the current
memory.

ARG is passed to `partial-recall--switch-to-buffer-function'."
  (interactive (list (partial-recall--complete-reality "Switch to moment: " t t)
                     current-prefix-arg))

  (funcall partial-recall--switch-to-buffer-function buffer arg))

;;;###autoload
(defun partial-recall-reclaim (buffer)
  "Reclaim BUFFER.

The selection is limited to buffers belonging to other memories.
The selected buffer is removed from that memory and subsequently
switched to."
  (interactive (list (partial-recall--complete-dream "Reclaim moment: ")))

  (when-let* ((reclaimed (partial-recall--reclaim buffer t))
              (buffer (partial-recall--moment-buffer reclaimed)))

    (partial-recall--switch-to-and-neglect buffer)))

;;;###autoload
(defun partial-recall-reject (buffer memory)
  "Reject BUFFER and push it to MEMORY.

The selection is limited to buffers belonging to the current
memory."
  (interactive (list (partial-recall--complete-reality "Refuse moment: ")
                     (partial-recall--complete-memory "Target memory: ")))

  (partial-recall--reject buffer memory))

;;;###autoload
(defun partial-recall-forget (buffer)
  "Forget BUFFER.

Removes the buffer from the current reality. The buffer can be
reclaimed afterwards."
  (interactive (list (partial-recall--complete-reality "Forget moment: ")))

  (partial-recall--forget buffer t))

;;;###autoload
(defun partial-recall-implant (buffer &optional excise)
  "Implant the BUFFER.

This will ensure the buffer is never removed from the memory.

If EXCISE is T, do that instead. The excised buffer can be
removed from the memory again."
  (interactive (list (partial-recall--complete-reality (if current-prefix-arg
                                                           "Excise moment: "
                                                         "Implant moment: "))
                     current-prefix-arg))

  (partial-recall--implant buffer excise))

;;;###autoload
(defun partial-recall-lift (buffer)
  "Lift BUFFER out of the subconscious and switch to it.

Buffers that are forgotten move to the subconscious first before
being removed completely. As long as the subconscious itself
isn't at capacity and needs to drop the oldest buffer, it can be
lifted."
  (interactive (list (partial-recall--complete-subconscious "Lift moment: ")))

  (partial-recall--lift buffer)

  (partial-recall--switch-to-and-neglect buffer))

;;;###autoload
(defun partial-recall-meld (a b &optional close)
  "Meld memories A and B.

This moves all moments from A to B. If CLOSE is t, the tab of
memory B is closed afterwards."
  (interactive (list (partial-recall--complete-memory "Meld source: ")
                     (partial-recall--complete-memory "Meld target: ")
                     current-prefix-arg))

  (partial-recall--meld a b close))

;;;###autoload
(defun partial-recall-flush (&optional arg)
  "Flush the current memory.

Removes impermanent and never updated moments.

Passes ARG to the underlying function which will be passed to the
`partial-recall-memorable-traits' functions."
  (interactive "P")

  (partial-recall--flush (partial-recall--reality) arg))

;;;###autoload
(defun partial-recall-next ()
  "Go to the next moment."
  (interactive)

  (when-let ((next (partial-recall--next-buffer)))

    (partial-recall--switch-to-and-neglect next)))

;;;###autoload
(defun partial-recall-previous ()
  "Go to the previous moment."
  (interactive)

  (when-let ((previous (partial-recall--previous-buffer)))

    (partial-recall--switch-to-and-neglect previous)))

;;;###autoload
(defun partial-recall-forget-some ()
  "Prompt the user to forget some moments."
  (interactive)

  (partial-recall--forget-some))

;;;###autoload
(defun partial-recall-explain-omission ()
  "Explain why the current buffer is non-meaningful."
  (interactive)

  (if-let ((explanation (partial-recall--explain-omission)))
      (message explanation)
    (message "Buffer is meaningful or infringed trait has no explanation")))

(provide 'partial-recall)

;;; partial-recall.el ends here

;;; partial-recall.el --- STM for `tab-bar-mode' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.13.1
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
;; forgotten, they can be made permanent and they can be reinforced.
;; All of these things happen automatically but can be performed
;; explicitly by the user as well.
;;
;; When moments are forgotten, their buffers are marked as belonging
;; to the subconscious (from where they can be lifted).

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)
(require 'eieio)

;;;; Customization

(defgroup partial-recall nil
  "Short-term (buffer) memory."
  :group 'partial-recall)

(defcustom partial-recall-handle-delay 3
  "The delay in seconds after which a buffer will be handled.

It should give you ample time to switch to another buffer before
handling. The underlying time is reset on every switch.

Buffers visited with a no-record flag aren't handled."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-memory-size 10
  "The amount of buffers to recall per tab.

This limit may temporarily increase if buffers are remembered in
quick succession. See `partial-recall-intermediate-term'."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-intermediate-term (* 20 60)
  "Threshold after which moments are handled differently.

This threshold is used in three places: (1) Moments at or above
this threshold can be reclaimed automatically. (2) Moments below
this value will not be flushed. (3) Moments below this threshold
will entail switching to their tab if
`partial-recall-auto-switch' is t.

See `partial-recall--reclaim', `partial-recall--gracedp' and
 `partial-recall--maybe-switch-memory'.

Can be set to nil to disable this behavior."
  :type '(choice (integer :tag "Number of seconds")
                 (const :tag "Don't use" nil))
  :group 'partial-recall)

(defcustom partial-recall-auto-switch t
  "Whether to automatically switch to a buffer's memory."
  :type '(choice (const :tag "Don't switch" nil)
                 (const :tag "Switch" t)
                 (const :tag "Prompt first" prompt))
  :group 'partial-recall)

(defcustom partial-recall-lighter-prefix "pr"
  "The prefix used in `partial-recall-lighter'."
  :type 'string
  :group 'partial-recall)

(defcustom partial-recall-lighter-show-info t
  "Whether to show information about memory and moment in lighter."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-record-triggers '(consult-buffer)
  "Commands that should trigger recording the buffer.

It will allow the buffer visited before these commands to be
considered visible. This is relevant for
`partial-recall-concentration--concentrate' which increases a
moment's focus."
  :type '(repeat symbol)
  :group 'partial-recall)

(defcustom partial-recall-log t
  "Whether to log.

This is either nil meaning no logging, or 1 for info logging and
0 for debug logging. Any other symbol also means info logging."
  :type '(choice (const :tag "No logging" nil)
                 (symbol :tag "Default")
                 (const :tag "Info" 1)
                 (const :tag "Debug" 0))
  :group 'partial-recall)

(defcustom partial-recall-log-echo nil
  "Whether to also log messages in the echo area."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-log-prefix "PR"
  "The prefix used for log messages."
  :type '(choice (const :tag "Text" "PR")
                 (const :tag "None" nil))
  :group 'partial-recall)

(defcustom partial-recall-filter '("COMMIT_EDITMSG" "git-rebase-todo")
  "Names of buffers that should be ignored.

See trait `partial-recall--not-filtered-p'."
  :type '(repeat regexp)
  :group 'partial-recall)

(defcustom partial-recall-meaningful-traits '(buffer-file-name
                                              partial-recall--not-filtered-p
                                              partial-recall--not-in-view-mode-p
                                              partial-recall--not-to-be-viewed-p
                                              partial-recall--not-banished-p)
  "List of functions that describe traits of a meaningful buffer.

These functions are inspected using `func-arity'. If they have a
minimum arity of at least 1 OR the symbol `many' for their
maximum arity, they will be called with `current-buffer' as the
first argument, otherwise they are called with no argument.

If any such function does not return a truthy value, the buffer
is not considered meaningful, meaning a buffer needs to satisfy
all predicates."
  :type '(repeat function)
  :group 'partial-recall)

(defcustom partial-recall-memorable-traits '(partial-recall--gracedp)
  "List of functions that determine a memorable moment.

These functions are called with moments up for suppression and
the current prefix argument.

If any such function does return a truthy value, the moment is
considered memorable. See `partial-recall--flush'."
  :type '(repeat function)
  :group 'partial-recall)

(defcustom partial-recall-intensities '((swap . -10) (reinsert . 20) (concentrate . 4))
  "The amount of focus gained (or lost) from actions.

See `partial-recall-moment--adjust' and its callers."
  :type '(alist :key-type symbol :value-type integer)
  :group 'partial-recall)

;;;; Variables

;;;;; Private variables

(defvar partial-recall--table (make-hash-table :test #'equal)
  "Hash table mapping tabs to memories using common key.")

(defvar partial-recall--schedule-timer nil
  "Timer that runs until a buffer will be handled.

Switching to a buffer just schedules its handling. Only the
buffer the user lingers on will actually be handled.")

(defvar partial-recall--last-focus nil
  "The moment that was last focused.")

(defvar partial-recall--last-handled nil
  "The buffer that was last handled.")

(defvar partial-recall--neglect nil
  "If t, the next visited buffer will not be handled.")

(defvar partial-recall--switch-to-buffer-function #'switch-to-buffer
  "Function to call when switching to a buffer.")
(defvar partial-recall--pop-to-buffer-function #'pop-to-buffer
  "Function to call when popping to a buffer.")

(defvar partial-recall--before-minibuffer nil
  "Buffer visited before minibuffer was set up.")

(defvar-local partial-recall--permanent nil
  "Whether the buffer's moment is permanent.

This is only set for extensions and doesn't affect internal
logic.")

(defvar partial-recall--restored-tab nil
  "The tab that was just restored.")

(defvar partial-recall--to-be-viewed nil
  "The buffer about to be viewed by `view-buffer'.")

(defvar-local partial-recall--banished nil
  "Whether the buffer should never be scheduled.

This has no effect if `partial-recall--not-banished-p' is not
part of `partial-recall-meaningful-traits'.")

;;;;; Maps

;;;###autoload
(defvar partial-recall-command-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "4") 'partial-recall-switch-to-buffer-other-window)
    (define-key map (kbd "b") 'partial-recall-switch-to-buffer)
    (define-key map (kbd "c") 'partial-recall-reclaim)
    (define-key map (kbd "f") 'partial-recall-forget)
    (define-key map (kbd "g") 'partial-recall-pop-to-logs)
    (define-key map (kbd "i") 'partial-recall-make-permanent)
    (define-key map (kbd "j") 'partial-recall-reject)
    (define-key map (kbd "k") 'partial-recall-forget-some)
    (define-key map (kbd "l") 'partial-recall-lift)
    (define-key map (kbd "m") 'partial-recall-menu)
    (define-key map (kbd "n") 'partial-recall-next)
    (define-key map (kbd "o") 'partial-recall-explain-omission)
    (define-key map (kbd "p") 'partial-recall-previous)
    (define-key map (kbd "r") 'partial-recall-remember)
    (define-key map (kbd "s") 'partial-recall-remember-some)
    (define-key map (kbd "t") 'partial-recall-retrieve)
    (define-key map (kbd "u") 'partial-recall-meld)
    (define-key map (kbd "x") 'partial-recall-flush)
    (define-key map (kbd "z") 'partial-recall-banish)

    map)
  "Map for `partial-recall-mode' commands.")

(defvar-keymap partial-recall-navigation-map
  :doc "Keymap to repeat navigation commands."
  :repeat t
  "n" 'partial-recall-next
  "p" 'partial-recall-previous)

;;;;; Faces

(defface partial-recall-emphasis
  '((t (:inherit (font-lock-type-face))))
  "Face used for emphasis."
  :group 'partial-recall)

(defface partial-recall-soothe
  '((t (:inherit (success))))
  "Face used to soothe."
  :group 'partial-recall)

(defface partial-recall-deemphasized
  '((t (:inherit (shadow))))
  "Face used to de-emphasize."
  :group 'partial-recall)

(defface partial-recall-contrast
  '((t (:inherit (warning))))
  "Face used for contrast."
  :group 'partial-recall)

(defface partial-recall-alert
  '((t (:inherit (error))))
  "Face used to alert."
  :group 'partial-recall)

;;;;; Hooks

(defvar partial-recall-before-probe-hook nil
  "Functions called before a memory was probed.

Each function will be called with the probed memory as its only
argument.")

(defvar partial-recall-after-probe-hook nil
  "Functions called after a memory was probed.

Each function will be called with the probed memory as its only
argument.")

(defvar partial-recall-permanence-change-hook nil
  "Functions called after a moment's permanence has changed.

Each function will be called with the moment and its new
permanence value as arguments.")

(defvar partial-recall-after-insert-hook nil
  "Functions called after a moment was inserted.

Each function will be called with the inserted moment as its only
argument.")

(defvar partial-recall-after-create-hook nil
  "Functions run after a memory is created.

Each function will be called with the name of the memory (that is
the name of the tab).")

(defvar partial-recall-after-reality-change-hook nil
  "Function to run after reality changes.

Each function is called with the name of the memory (that is the
name of the tab).")

(defvar partial-recall-after-focus-change-hook nil
  "Function to run after focus changes.

Each function will be called with the moment that gained focus.")

;;;; Structures

(cl-defstruct (partial-recall-moment
               (:conc-name partial-recall-moment--)
               (:constructor partial-recall-moment--create
                             (buffer
                              &aux
                              (timestamp (floor (time-to-seconds)))
                              (focus 0)
                              (permanence nil))))
  "A moment of partial recall.

A moment is defined by a buffer, a timestamp when that buffer was
first remembered, a count of how many times it was updated and a
permanence marker that can prevent it from being forgotten if
`partial-recall-plasticity-of-moment' is on.

The timestamp is distinct from `buffer-display-time' and the
focus is distinct from `buffer-display-count'."
  buffer timestamp focus permanence)

(cl-defstruct (partial-recall-memory
               (:conc-name partial-recall-memory--)
               (:constructor partial-recall-memory--create
                             (key
                              &key
                              (orig-size partial-recall-memory-size)
                              &aux
                              (moments (make-ring orig-size)))))
  "A memory of partial recall.

A memory is a key that connects it to the hash table, a ring of
moments and the size it had upon construction."
  key orig-size moments)

;;;; Utility

;;;;; Key creation and look-up

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

(defun partial-recall--memory-by-key (key &optional size)
  "Get or create memory identified by KEY.

If not found, the memory is created, optionally using size SIZE."
  (when key
    (if-let* ((table partial-recall--table)
              (memory (gethash key table)))

        memory

      (let* ((size (or size partial-recall-memory-size))
             (new-memory (partial-recall-memory--create key :orig-size size)))

        (puthash key new-memory table)
        new-memory))))

;;;;; Rings

(defun partial-recall--ring-oldest (ring)
  "Get the oldest element in RING."
  (unless (ring-empty-p ring)
    (ring-ref ring (1- (ring-length ring)))))

(defun partial-recall--ring-youngest (ring)
  "Get the oldest element in RING."
  (unless (ring-empty-p ring)
    (ring-ref ring 0)))

(defun partial-recall--ring-insert (ring item &optional extend)
  "Insert ITEM in RING.

If EXTEND is t, also extend."
  (ring-insert+extend ring item extend)

  (run-hook-with-args 'partial-recall-after-insert-hook item)

  item)

;;;;; Windows

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
      (partial-recall-warn "Function `%s' has the wrong arity" fun)
      t)))

;;;;; Frames

(defmacro partial-recall--in-other-frame (memory &rest forms)
  "Evaluate FORMS in other frame.

If MEMORY is not in another form, this is a no-op."
  (declare (indent defun))

  `(and-let* ((partial-recall--foreignp ,memory)
              (info (partial-recall-memory--find-in-other-frame ,memory))
              (frame (plist-get info :frame)))

     (partial-recall-debug "Evaluating on behalf of `%s' in other frame" ,memory)

     (with-selected-frame frame
       ,@forms)))

(defun partial-recall--is-other-frame-p (frame)
  "Check that FRAME is not the selected frame."
  (not (eq frame (selected-frame))))

(defun partial-recall--foreignp (memory)
  "Check if MEMORY belongs to foreign frame."
  (null (partial-recall--find-tab-from-memory memory)))

;;;; Accessors

;;;;; Memories

(defun partial-recall-memories ()
  "Get all memories."
  (hash-table-values partial-recall--table))

(defun partial-recall-memory--owns-buffer-p (memory buffer)
  "Check if MEMORY owns BUFFER."
  (let ((moments (partial-recall-memory--moments memory)))

    (catch 'found
      (dotimes (ind (ring-length moments))
        (when (equal buffer (partial-recall-moment--buffer (ring-ref moments ind)))
          (throw 'found ind))))))

(defun partial-recall-memory--remove-buffer (buffer memory)
  "Remove BUFFER from MEMORY and return it."
  (when-let* ((moments (partial-recall-memory--moments memory))
              (index (partial-recall-memory--owns-buffer-p memory buffer))
              (removed (ring-remove moments index)))

    removed))

(defun partial-recall-memory--find-in-other-frame (memory)
  "Get tab and frame of MEMORY."
  (when-let* ((other-frames (filtered-frame-list #'partial-recall--is-other-frame-p))
              (found (catch 'found
                       (dotimes (ind (length other-frames))
                         (when-let* ((frame (nth ind other-frames))
                                     (tab (partial-recall--find-tab-from-memory memory frame)))

                           (throw 'found (list :tab tab :frame frame)))))))
    found))

(defun partial-recall-memory--has-buffers-p (&optional memory)
  "Check if the MEMORY has buffers."
  (when-let* ((memory (or memory (partial-recall--reality)))
              (moments (partial-recall-memory--moments memory)))

    (not (ring-empty-p moments))))

(defun partial-recall-memory--name (memory)
  "Get the name of MEMORY."
  (partial-recall--find-any-tab-from-memory memory))

(defun partial-recall-memory--unique (memory)
  "Get the unique name of MEMORY.

This is just indirection to access the key slot."
  (partial-recall-memory--key memory))

(defun partial-recall-memory--removed-buffers (&optional memory)
  "Get a list of buffers removed from MEMORY."
  (let* ((memory (or memory (partial-recall--reality)))
         (buffers (partial-recall--suppressed (partial-recall-memory--unique memory))))

    buffers))

(defun partial-recall--reality ()
  "Get the current memory."
  (partial-recall--memory-by-key (partial-recall--key)))

(defun partial-recall--realityp (memory)
  "Check if MEMORY is the reality."
  (eq (partial-recall--reality) memory))

(defun partial-recall-memory--near-capacity-p (memory &optional threshold)
  "Check if MEMORY is near capacity.

Optional THRESHOLD determines what this means. If it is not
passed, this checks if the memory is exactly at capacity. Passing
a positive integer determines if the capacity is near that
threshold."
  (when-let ((threshold (or threshold 0))
             (ring (partial-recall-memory--moments memory)))

    (>= (ring-length ring)
        (- (ring-size ring) threshold))))

;;;;; Moments

(defun partial-recall-moments ()
  "Get all moments."
  (cl-loop for _k being the hash-keys of partial-recall--table
           using (hash-values memory)
           append (ring-elements (partial-recall-memory--moments memory))))

(defun partial-recall-moment--set-permanence (moment permanence)
  "Set MOMENT PERMANENCE."
  (setf (partial-recall-moment--permanence moment) permanence)

  (with-current-buffer (partial-recall-moment--buffer moment)
    (setq-local partial-recall--permanent permanence))

  (run-hook-with-args 'partial-recall-permanence-change-hook moment permanence)

  moment)

(defun partial-recall-moment--update-timestamp (moment)
  "Update the timestamp for MOMENT."
  (setf (partial-recall-moment--timestamp moment) (floor (time-to-seconds)))

  moment)

(defun partial-recall-moment--adjust-focus (moment context)
  "Adjust the focus for MOMENT using CONTEXT.

Permanent moments do not gain additional focus."
  (and-let* ((amount (or (alist-get context partial-recall-intensities) 0))
             ((or (not (partial-recall-moment--permanence moment))
                  (< amount 0)))
             (count (partial-recall-moment--focus moment))
             (updated-count (max 0 (+ count amount))))

    (setf (partial-recall-moment--focus moment) updated-count)

    (run-hook-with-args 'partial-recall-after-focus-change-hook moment updated-count)

    moment))

(defun partial-recall-moment--adjust (moment &optional context)
  "Intensify MOMENT.

This will update its timestamp and increment its focus.

If CONTEXT has a value in `partial-recall-intensities', increase
by that amount."
  (partial-recall-moment--adjust-focus moment context)

  (partial-recall-moment--update-timestamp moment))

(defun partial-recall-moment--reset-count (moment)
  "Reset the focus for MOMENT."
  (setf (partial-recall-moment--focus moment) 0)

  moment)

;;;;; Buffers

(defun partial-recall-buffers ()
  "Get all mapped buffers."
  (mapcar #'partial-recall-moment--buffer (partial-recall-moments)))

(defun partial-recall--buffer-in-memory-p (buffer &optional memory)
  "Check if BUFFER is a member of MEMORY.

If MEMORY is not passed, use the current reality."
  (partial-recall-memory--owns-buffer-p (or memory (partial-recall--reality)) buffer))

(defun partial-recall--buffer-mapped-p (buffer)
  "Check if BUFFER is mapped."
  (let ((buffers (partial-recall-buffers)))

    (memq buffer buffers)))

(defun partial-recall--buffer-visible-p (buffer)
  "Check that BUFFER remains visible.

This also checks for buffers that might have been obscured."
  (let* ((windows (partial-recall--window-list))
         (visible (and buffer
                       (buffer-live-p buffer)
                       (or (eq buffer partial-recall--before-minibuffer)
                           (memq buffer (mapcar #'window-buffer windows))))))

    visible))

(defun partial-recall--buffer-new-p (buffer)
  "Check if BUFFER is actually new."
  (cond
   ((eq partial-recall--last-handled buffer)
    nil)
   ((eq partial-recall--neglect buffer)
    (setq partial-recall--neglect nil))
   (t t)))

(defun partial-recall--buffer-equals-moment-p (buffer moment)
  "Check if BUFFER is encapsulated by MOMENT."
  (eq (partial-recall-moment--buffer moment) buffer))

(defun partial-recall--buffer-focus (&optional buffer)
  "Get the focus of BUFFER."
  (when-let* ((buffer (or buffer (current-buffer)))
              (moment (partial-recall--find-owning-moment buffer)))

    (partial-recall-moment--focus moment)))

(defun partial-recall--find-owning-memory (&optional buffer)
  "Return the memory that owns BUFFER.

Defaults to the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (memories (partial-recall-memories))
         (find (apply-partially #'partial-recall--buffer-in-memory-p buffer)))

    (seq-find find memories)))

(defun partial-recall--find-owning-moment (buffer &optional memory)
  "Get the moment that encapsulates BUFFER.

Searches all memories unless MEMORY is provided."
  (when-let* ((memory (or memory
                          (let ((memories (partial-recall-memories))
                                (find-memory (apply-partially #'partial-recall--buffer-in-memory-p buffer)))
                            (seq-find find-memory memories))))
              (ring (partial-recall-memory--moments memory))
              (index (partial-recall-memory--owns-buffer-p memory buffer)))

    (ring-ref ring index)))

(defun partial-recall-current-moment ()
  "Get the current buffer's moment."
  (partial-recall--find-owning-moment (current-buffer)))

;;;;; Tabs

(defun partial-recall--find-tab-from-memory (memory &optional frame)
  "Get the tab for MEMORY.

Optionally search in FRAME."
  (when-let ((key (partial-recall-memory--unique memory))
             (tabs (if frame
                       (with-selected-frame frame
                         (funcall tab-bar-tabs-function frame))
                     (funcall tab-bar-tabs-function))))

    (seq-find (lambda (it) (string= key (alist-get 'pr it))) tabs)))

(defun partial-recall--find-any-tab-from-memory (memory)
  "Get the tab name for MEMORY searching all frames."
  (let ((frames (frame-list)))
    (catch 'found
      (dotimes (ind (length frames))
        (when-let* ((frame (nth ind frames))
                    (name (partial-recall--find-tab-name-from-memory memory frame)))

          (throw 'found name))))))

(defun partial-recall--find-tab-name-from-memory (&optional memory frame)
  "Get the tab name for MEMORY.

Optionally search in FRAME."
  (when-let ((memory (or memory (partial-recall--reality)))
             (tab (partial-recall--find-tab-from-memory memory frame)))

    (alist-get 'name tab)))

(defun partial-recall--fix-up-primary-tab ()
  "Fix up the primary tab."
  (if-let* ((mode tab-bar-mode)
            (tabs (funcall tab-bar-tabs-function))
            (original (nth 0 tabs)))

      (unless (partial-recall--key original)
        (partial-recall--on-create original))

    (partial-recall-warn "Might have failed to set up original tab")))

(defun partial-recall--queue-tab-fix-up (&rest _r)
  "Queue a fix-up of the original tab."
  (run-at-time 1.0 nil #'partial-recall--fix-up-primary-tab))

;;;; Handlers

(defun partial-recall--schedule-buffer (buffer)
  "Schedule handling BUFFER."
  (and-let* ((buffer (get-buffer buffer))
             ((partial-recall--buffer-new-p buffer))
             ((partial-recall--meaningful-buffer-p buffer)))

    (partial-recall--void-schedule-timer)

    (partial-recall-debug "Scheduling buffer `%s'" buffer)

    (setq partial-recall--schedule-timer
          (run-at-time
           partial-recall-handle-delay
           nil
           #'partial-recall--handle-buffer buffer))))

(defun partial-recall--void-schedule-timer ()
  "Void the current timer."
  (when partial-recall--schedule-timer
    (unless (timer--triggered partial-recall--schedule-timer)

      (partial-recall-debug "Canceling previous timer `%s'" partial-recall--schedule-timer)

      (cancel-timer partial-recall--schedule-timer))

    (setq partial-recall--schedule-timer nil)))

(defun partial-recall--handle-buffer (buffer)
  "Handle BUFFER.

This will remember new buffers and maybe reclaim mapped buffers.
If in between scheduling and handling the buffer it can no longer
be found, it will be ignored."
  (partial-recall--void-schedule-timer)

  (if (partial-recall--buffer-visible-p buffer)

      (progn
        (partial-recall-log "Handling buffer `%s' as it remains visible" buffer)

        (if (partial-recall--buffer-mapped-p buffer)
            (partial-recall--recollect buffer)
          (partial-recall--remember buffer))

        (setq partial-recall--last-handled buffer))

    (partial-recall-log "Not handling `%s' as it is no longer visible" buffer)))

;;;;; Reactions

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

    (setcdr tab (push (cons 'pr key) state))

    (run-hook-with-args 'partial-recall-after-create-hook (alist-get 'name tab))))

(defun partial-recall--on-close (tab only)
  "Remove TAB from table if it is not the ONLY one."
  (and-let* (((not only))
             (tab-key (partial-recall--key tab))
             (table partial-recall--table)
             (memory (gethash tab-key table))
             (moments (partial-recall-memory--moments memory)))

    (dolist (it (ring-elements moments))
      (partial-recall--clean-up-buffer (partial-recall-moment--buffer it))
      (partial-recall--suppress it memory))

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
                          (not (partial-recall--buffer-in-memory-p (window-buffer it))))
                        (window-list))))

    (partial-recall--maybe-switch-memory (window-buffer foreign) t)))

(defun partial-recall--after-tab-bar-switch (name)
  "Call hook with NAME."
  (run-hook-with-args 'partial-recall-after-reality-change-hook name))

(defun partial-recall--before-undo-close-tab ()
  "Record the tab that will be undone and record its key."
  (and-let* ((predicate (lambda (it) (frame-live-p (alist-get 'frame it))))
             (closed (seq-find predicate tab-bar-closed-tabs))
             (tab (alist-get 'tab closed)))

    (setq partial-recall--restored-tab (alist-get 'pr tab))))

(defun partial-recall--after-undo-close-tab ()
  "Lift subconscious buffers."
  (and-let* ((key partial-recall--restored-tab)
             (buffers (partial-recall--suppressed key))
             (predicate (lambda (a b)
                          (not (time-less-p (buffer-local-value 'buffer-display-time a)
                                            (buffer-local-value 'buffer-display-time b)))))
             (sorted (sort buffers predicate))
             (truncated (seq-subseq sorted 0
                                    (min (length sorted)
                                         partial-recall-memory-size)))

             (memory (partial-recall--memory-by-key key))
             (moments (partial-recall-memory--moments memory)))


    (dolist (buffer truncated)
      (partial-recall--clear-remnant buffer)
      (partial-recall--clear-disturbed buffer)
      (ring-insert moments (partial-recall-moment--create buffer)))))

(defun partial-recall--before-view-buffer (buffer &rest _)
  "Record BUFFER as about to be viewed."
  (setq partial-recall--to-be-viewed buffer))

;;;; Verbs
;;
;; This section holds the core of the package, namely the verbs that
;; change state.

(defun partial-recall--remember (buffer)
  "Remember BUFFER as belonging to the current reality.

This will either create a new moment for the buffer or reclaim
one from the subconscious.

If BUFFER is already owned by the current reality, this is a
no-op."
  (and-let* ((memory (partial-recall--reality))
             (ring (partial-recall-memory--moments memory))
             ((not (partial-recall-memory--owns-buffer-p memory buffer))))

    (partial-recall--probe-memory memory)

    (partial-recall--clear-remnant buffer)
    (partial-recall--clear-disturbed buffer)

    (let ((moment (partial-recall-moment--create buffer)))

      (partial-recall--ring-insert ring moment))))

(defvar-local partial-recall--disturbed nil
  "If non-nil don't include during `partial-recall--remember-some'.")

(defun partial-recall--clear-disturbed (&optional buffer)
  "Clear disturbed marker from BUFFER."
  (with-current-buffer buffer
    (setq partial-recall--disturbed nil)))

(defun partial-recall--remember-some (&optional include-all)
  "Remember some buffers that were lost.

This will prompt the user for each lost buffer exactly once unless
optional INCLUDE-ALL is t."
  (let ((buffers (partial-recall-memory--removed-buffers)))

    (dolist (buffer buffers)
      (if (and (or include-all
                   (not (buffer-local-value 'partial-recall--disturbed buffer)))
               (yes-or-no-p (format "Remember lost buffer `%s'?" buffer)))
          (partial-recall--remember buffer)
        (with-current-buffer buffer
          (setq partial-recall--disturbed t))))))

(defun partial-recall--reinforce (buffer)
  "Reinforce BUFFER in reality.

This will re-insert the buffer's moment in the current reality.

If this buffer isn't already part of the current reality, this is
a no-op."
  (and-let* ((reality (partial-recall--reality))
             (moment (partial-recall--find-owning-moment buffer reality)))

    (partial-recall--reinsert moment reality "reinforcement")))

(defun partial-recall--reinsert (moment memory &optional reason)
  "Reinsert MOMENT into MEMORY.

This removes, inserts and extends the memory. The moment is
intensified.

If the moment doesn't belong to memory, this is a no-op.

Optionally, give a REASON why moment was re-inserted."
  (and-let* ((ring (partial-recall-memory--moments memory))
             ((ring-member ring moment))
             (buffer (partial-recall-moment--buffer moment))
             (reason (or reason "unknown reason")))

    (partial-recall-debug "Re-inserting `%s' in `%s' (%s)" moment memory reason)

    (partial-recall-moment--adjust moment 'reinsert)

    (ring-remove+insert+extend ring moment t)))

(defun partial-recall--reclaim (buffer &optional force)
  "Reclaim BUFFER for the current reality.

This will move the buffer's moment from another memory provided
that it is neither permanent nor short-term. This works across
frames.

If FORCE is t it will be reclaimed either way.

If the buffer already belongs to the current reality, this is a
no-op."
  (if-let* ((reality (partial-recall--reality))
            (owner (partial-recall--find-owning-memory buffer))
            ((not (eq reality owner)))

            (moment (partial-recall--find-owning-moment buffer owner))
            ((or force
                 (and
                  (not (partial-recall-moment--permanence moment))
                  (partial-recall--intermediate-term-p moment)))))

      (progn
        (partial-recall--in-other-frame owner
          (partial-recall--clean-up-window buffer))

        (partial-recall--swap owner reality moment))

    (partial-recall-debug "Won't claim `%s'" buffer)))

(defun partial-recall--forget (&optional buffer banish)
  "Forget BUFFER.

This will remove the buffer's moment from its owning memory and
clean-up remaining windows. The moment is probed afterwards.

If BANISH is t, the buffer is marked with
`partial-recall--banished', meaning it will no longer be
scheduled automatically.

BUFFER defaults to the current buffer no buffer is passed."
  (when-let* ((buffer (or buffer (current-buffer)))
              ((partial-recall--buffer-mapped-p buffer))
              (maybe-remove (lambda (_key memory)
                              (when-let ((moment (partial-recall-memory--remove-buffer buffer memory)))

                                (partial-recall--suppress moment memory)

                                (partial-recall--probe-memory memory)

                                (throw 'found memory)))))

    (partial-recall--clean-up-buffer buffer)

    (when banish
      (partial-recall--banish buffer))

    (let ((memory (catch 'found
                    (maphash maybe-remove partial-recall--table))))

      (partial-recall-log "`%s' was removed from `%s'" buffer memory))))

(defun partial-recall--forget-some ()
  "Forget some moments.

This works like `kill-some-buffers' and will prompt the user to
forget each moment while indicating if it has been modified."
  (let ((moments (partial-recall-moments)))

    (while moments
      (and-let* ((moment (car moments))
                 (buffer (partial-recall-moment--buffer moment))
                 (name (buffer-name buffer))
                 ((not (string-equal name ""))))


        (when (yes-or-no-p (format "Forget `%s' (%s)?"
                                   (partial-recall-repr moment)
                                   (if (buffer-modified-p buffer)
                                       "modified"
                                     "unmodified")))

          (partial-recall--forget buffer))

        (setq moments (cdr moments))))))

(defun partial-recall--banish (buffer)
  "Banish BUFFER."
  (with-current-buffer buffer
    (setq partial-recall--banished t)))

(defun partial-recall--reject (buffer memory)
  "Reject BUFFER and push it to MEMORY.

This is the opposite of `partial-recall--reclaim'.

This routine signals errors if the memory to push to is the
current reality or if BUFFER doesn't belong to MEMORY."
  (let ((reality (partial-recall--reality)))

    (when (equal memory reality)
      (user-error "The current reality can't be the target of the rejected buffer"))

    (when (not (equal (partial-recall--find-owning-memory buffer)
                      reality))
      (user-error "The buffer to reject does not belong to the current reality"))

    (when-let ((moment (partial-recall--find-owning-moment buffer)))

      (partial-recall--swap reality memory moment)
      (partial-recall--clean-up-buffer buffer))))

(defun partial-recall--set-permanence (&optional buffer excise keep-focus)
  "Implant BUFFER.

This makes the buffer's owning moment permanent. A permanent
moment can not be reclaimed and will not be automatically
forgotten.

If EXCISE is t, remove permanence instead.

If KEEP-FOCUS is t, do not reset the focus."
  (when-let* ((buffer (or buffer (current-buffer)))
              (moment (partial-recall--find-owning-moment buffer))
              (verb (if excise "Removing permanence from" "Adding permanence to")))

    (unless (eq (partial-recall-moment--permanence moment) (not excise))

      (partial-recall-log "%s `%s'" verb moment)

      (partial-recall-moment--set-permanence moment (not excise))

      (when (and excise (not keep-focus))
        (partial-recall-moment--reset-count moment)))))

;;;;; Subconscious

(defvar-local partial-recall--remnant nil
  "This holds the key to the memory this buffer belonged to.")

(defun partial-recall--clear-remnant (&optional buffer)
  "Clear remnant from BUFFER."
  (with-current-buffer buffer
    (setq partial-recall--remnant nil)))

(defun partial-recall--suppress (moment memory)
  "Mark the buffer of MOMENT as suppressed by MEMORY."
  (with-current-buffer (partial-recall-moment--buffer moment)
    (setq-local partial-recall--remnant
                (partial-recall-memory--unique memory))))

(defun partial-recall--suppressed (&optional key)
  "Get the suppressed buffers.

If KEY is given, it is used, otherwise the current reality's key
is used."
  (let* ((key (or key (partial-recall--key)))
         (buffers (buffer-list)))

    (cl-loop for buffer in buffers
             if (string= key (buffer-local-value 'partial-recall--remnant buffer))
             collect buffer)))

(defun partial-recall--suppressed-p (buffer)
  "Check if BUFFER is currently suppressed."
  (memq buffer (partial-recall--suppressed)))

(defun partial-recall--recollect (buffer)
  "Recollect BUFFER.

If the buffer belongs to the current reality, it is reinforced.
Otherwise it is reclaimed.

This routine raises an error if it is called with an unmapped
buffer."
  (unless (partial-recall--buffer-mapped-p buffer)
    (user-error "Can't recollect an unmapped buffer"))

  (if (partial-recall--buffer-in-memory-p buffer)
      (partial-recall--reinforce buffer)
    (partial-recall--reclaim buffer)))

(defun partial-recall--swap (a b moment)
  "Swap MOMENT from memory A to B.

Both memories will be probed after the moment was move was completed."
  (and-let* ((a-ring (partial-recall-memory--moments a))
             (b-ring (partial-recall-memory--moments b))
             (index (ring-member a-ring moment))
             (removed (ring-remove a-ring index)))

    (partial-recall-log "Swapping `%s' from `%s' to `%s'" moment a b)

    (partial-recall--probe-memory a)
    (partial-recall--probe-memory b)

    (partial-recall-moment--adjust moment 'swap)

    (partial-recall--ring-insert b-ring removed)))

(defun partial-recall--meld (a b &optional close)
  "Meld memories A and B.

This moves all of the moments of A to B.

If CLOSE is t, the tab of B is closed afterwards.

This routine signals an error if A and B are the same memory."
  (when (eq a b)
    (user-error "Can't shift moments from identical memories"))

  (let ((moments-a (partial-recall-memory--moments a))
        (moments-b (partial-recall-memory--moments b)))

    (while (not (ring-empty-p moments-a))
      (let ((moment (ring-remove moments-a)))
        (partial-recall--ring-insert moments-b moment t)))

    (when close
      (tab-bar-close-tab-by-name (partial-recall--find-tab-name-from-memory a)))))

(defun partial-recall--flush (memory &optional arg ignore-visible)
  "Flush MEMORY.

This will call all functions of `partial-recall-memorable-traits'
to check if a moment should be kept, passing moment and ARG.

If IGNORE-VISIBLE is t, currently displayed moments will not be flushed.

Buffers that aren't kept are suppressed and have their windows
cleaned up."
  (let* ((ring (partial-recall-memory--moments memory))
         (count 0)
         (moments (ring-elements ring))
         (filter (lambda (moment) (not (partial-recall--buffer-visible-p (partial-recall-moment--buffer moment)))))
         (moments (if ignore-visible (seq-filter filter moments) moments)))

    (dolist (moment moments)
      (unless (seq-some (lambda (it) (funcall it moment arg)) partial-recall-memorable-traits)

        (setq count (1+ count))

        (let* ((index (ring-member ring moment))
               (removed (ring-remove ring index)))

          (partial-recall-debug "Flushing %s" removed)

          (partial-recall--suppress removed memory))

        (partial-recall--clean-up-buffer (partial-recall-moment--buffer moment))))

    (when (> count 0)
      (partial-recall-log "Flushed %d moments from `%s'" count memory)

      (partial-recall--probe-memory memory))

    count))

(defun partial-recall--spin-out (buffers)
  "Spin out BUFFERS into new memory."
  (when-let* ((prompt (format "Spin out %d memories to tab: " (length buffers)))
              (name (read-string prompt)))

    (tab-bar-new-tab)
    (tab-bar-rename-tab name)

    (switch-to-buffer (car buffers) t)

    (dolist (buf buffers)
      (partial-recall--reclaim buf t))))

;;;;; Repercussions

(defun partial-recall--probe-memory (memory)
  "Probe MEMORY.

This will run the two memory hooks before and after maybe
suppressing the oldest moment."
  (run-hook-with-args 'partial-recall-before-probe-hook memory)

  (partial-recall--maybe-suppress-oldest-moment memory)

  (run-hook-with-args 'partial-recall-after-probe-hook memory))

(defun partial-recall--maybe-suppress-oldest-moment (memory)
  "Suppress the oldest moment in MEMORY if necessary.

This will be any moment that would be removed anyway by insertion
beyond the memory's limit."
  (and-let* (((partial-recall-memory--near-capacity-p memory))
             (ring (partial-recall-memory--moments memory))
             ((not (zerop (ring-length ring))))
             (removed (ring-remove ring)))

    (partial-recall--suppress removed memory)))

(defun partial-recall--maybe-switch-memory (&optional buffer unscheduled)
  "Maybe switch to BUFFER's memory.

Memories in the subconscious are not considered.

If UNSCHEDULED is t don't account for reclaiming."
  (and-let* (partial-recall-auto-switch
             (buffer (or buffer (current-buffer)))
             ((partial-recall--buffer-mapped-p buffer))
             ((not (partial-recall--buffer-in-memory-p buffer)))
             (moment (partial-recall--find-owning-moment buffer))
             ((or unscheduled
                  (partial-recall--short-term-p moment t)))
             (owner (partial-recall--find-owning-memory buffer))
             ((pcase partial-recall-auto-switch
                ('prompt
                 (yes-or-no-p (format "Switch to `%s'?" (partial-recall-repr owner))))
                ('t t)
                (_ nil))))

    (partial-recall-debug "Automatically switching to `%s' because of `%s'" owner moment)

    (with-current-buffer buffer
      (tab-bar-switch-to-tab (partial-recall--find-tab-name-from-memory owner)))))

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

    (when (eq partial-recall--last-handled buffer)
      (setq partial-recall--last-handled nil))

    (when (and partial-recall--last-focus
               (eq (partial-recall-moment--buffer partial-recall--last-focus)
                   buffer))
      (setq partial-recall--last-focus nil))))

;;;; Buffer switching

(defun partial-recall--switch-to-buffer-and-neglect (buffer)
  "Switch to BUFFER and make sure it is neglected.

This means the buffer won't be scheduled for handling."
  (setq partial-recall--neglect buffer)
  (funcall partial-recall--switch-to-buffer-function buffer))

(defun partial-recall--previous-buffer ()
  "Get the previous moment."
  (when-let* ((memory (partial-recall--reality))
              (current (partial-recall--find-owning-moment (current-buffer) memory))
              (previous (ring-previous (partial-recall-memory--moments memory) current)))

    (partial-recall-moment--buffer previous)))

(defun partial-recall--next-buffer ()
  "Get the next moment."
  (when-let* ((memory (partial-recall--reality))
              (current (partial-recall--find-owning-moment (current-buffer) memory))
              (next (ring-next (partial-recall-memory--moments memory) current)))

    (partial-recall-moment--buffer next)))

;;;; Traits

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

(defun partial-recall--not-to-be-viewed-p (buffer)
  "Make sure that BUFFER is not to be viewed."
  (let ((before partial-recall--to-be-viewed))

    (setq partial-recall--to-be-viewed nil)

    (not (eq buffer before))))

(defun partial-recall--not-banished-p (buffer)
  "Make sure that BUFFER is not banished."
  (not (buffer-local-value 'partial-recall--banished buffer)))

(defun partial-recall--gracedp (moment &optional arg)
  "Check if MOMENT was graced.

This holds true only for short-term moments.

If ARG is t, the current moment is considered graced as well."
  (or (partial-recall--short-term-p moment)
      (and arg
           (eq (current-buffer) (partial-recall-moment--buffer moment)))))

(defun partial-recall--explain-omission (&optional buffer)
  "Get the reason why BUFFER is (or would normally be) omitted.

This will try to find the first trait in
`partial-recall-meaningful-traits' that returns falsy and return
its explainer (property
`partial-recall-non-meaningful-explainer')if it exists."
  (and-let* ((buffer (or buffer (current-buffer)))
             (verify (apply-partially 'partial-recall--maybe-call-with-buffer buffer))
             (finder (lambda (it) (not (funcall verify it))))
             (failing (seq-find finder partial-recall-meaningful-traits))
             (explainer (get failing 'partial-recall-non-meaningful-explainer)))

    (if (partial-recall--buffer-mapped-p (current-buffer))
        (format "%s (was remembered explicitly)" explainer)
      explainer)))

(put 'buffer-file-name
     'partial-recall-non-meaningful-explainer
     "Buffer not associated with a file")
(put 'partial-recall--not-filtered-p
     'partial-recall-non-meaningful-explainer
     "Buffer is filtered by `partial-recall-filter'")
(put 'partial-recall--not-in-view-mode-p
     'partial-recall-non-meaningful-explainer
     "Buffer is in `view-mode'")
(put 'partial-recall--not-banished-p
     'partial-recall-non-meaningful-explainer
     "Buffer was banished.")

;;;; Thresholds

(defun partial-recall--equals-or-exceeds-p (moment threshold)
  "Check if MOMENT's age equals or exceeds THRESHOLD.

If SUB is a number, subtract it from threshold."
  (let ((ts (- (floor (time-to-seconds))
               (partial-recall-moment--timestamp moment))))

    (<= threshold ts)))

(defun partial-recall--falls-below-p (moment threshold &optional sub)
  "Check if MOMENT's age falls below THRESHOLD.

If SUB is a number, subtract it from the timestamp."
  (let ((ts (- (floor (time-to-seconds))
               (or sub 0)
               (partial-recall-moment--timestamp moment))))

    (> threshold ts)))

(defun partial-recall--short-term-p (moment &optional consider-delay)
  "Check if MOMENT is short-term.

If CONSIDER-DELAY is t, consider handling delay."
  (let ((sub (if consider-delay partial-recall-handle-delay 0)))

    (and (numberp partial-recall-intermediate-term)
         (partial-recall--falls-below-p moment partial-recall-intermediate-term sub))))

(defun partial-recall--intermediate-term-p (moment)
  "Check if MOMENT is intermediate."
  (and (numberp partial-recall-intermediate-term)
       (partial-recall--equals-or-exceeds-p moment partial-recall-intermediate-term)))

;;;; Graphing

(defvar partial-recall-graph--blocks ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"]
  "Vector of strings that represent focus visually.")
(defvar partial-recall-graph--ratios '(0.125 0.25 0.375 0.5 0.625 0.75 0.875 1)
  "Vector of floats that map absolute values to blocks.")

(defun partial-recall-graph (val max)
  "Graph VAL.

Selects a symbol based on VAL's relation to MAX."
  (let ((index 0)
        (max (or max 0))
        (max-index (1- (length partial-recall-graph--blocks))))

    (unless (or (zerop val) (zerop max))
      (while (and (> val (* max (nth index partial-recall-graph--ratios)))
                  (< index max-index))
        (setq index (1+ index)))

      (aref partial-recall-graph--blocks index))))

;;;; Messaging

(defvar partial-recall-log--buffer-name "*partial-recall-log*"
  "The buffer that holds all written logs.")

(defun partial-recall-log--write-to-buffer (fmt &rest args)
  "Format FMT with ARGS."
  (let ((buffer (get-buffer partial-recall-log--buffer-name))
        (inhibit-read-only t)
        (time-stamp (format-time-string "[%H:%M:%S] ")))

    (unless buffer
      (setq buffer (get-buffer-create partial-recall-log--buffer-name))
      (with-current-buffer buffer
        (read-only-mode)))

    (with-current-buffer buffer
      (goto-char (point-max))
      (insert time-stamp)
      (insert (apply #'format fmt args))
      (insert "\n")
      (set-buffer-modified-p nil))))

(defun partial-recall-warn (message &rest args)
  "Warn about MESSAGE.

Message will be formatted with ARGS."
  (display-warning 'partial-recall (apply #'format message args) :warning))

(defun partial-recall-debug (fmt &rest args)
  "Use ARGS to format FMT if debug is enabled."
  (when (and (numberp partial-recall-log)
             (< partial-recall-log 1))
    (apply 'partial-recall-log fmt args)))

(defun partial-recall-log (fmt &rest args)
  "Use ARGS to format FMT if not silenced.

If logging is enabled, they are written to the
`partial-recall-log--buffer-name' buffer. If `partial-recall-log-echo'
they are also echoed using `message'."
  (when partial-recall-log
    (let ((args (mapcar #'partial-recall-repr args)))

      (apply 'partial-recall-log--write-to-buffer fmt args)

      (when partial-recall-log-echo
        (let* ((message-log-max nil)
               (prefixed (partial-recall-log--prefix-fmt-string fmt)))

          (apply 'message prefixed args))))))

(defun partial-recall-log--prefix-fmt-string (format-string)
  "Prefix FORMAT-STRING."
  (if partial-recall-log-prefix
      (concat (propertize partial-recall-log-prefix 'face 'partial-recall-emphasis) " :: " format-string)
    format-string))

;;;; Representation

(defun partial-recall-repr (thing)
  "Format THING if it's a custom structure."
  (pcase (type-of thing)

    ('partial-recall-moment
     (let ((buffer (partial-recall-moment--buffer thing))
           (ts (partial-recall-moment--timestamp thing)))

       (format
        "#<moment %s (%s)>"
        (buffer-name buffer)
        (format-time-string "%H:%M:%S" (seconds-to-time ts)))))

    ('partial-recall-memory
     (let ((ring (partial-recall-memory--moments thing))
           (name (partial-recall-memory--name thing)))

       (format
        "#<memory %s (%d/%d)>"
        name
        (ring-length ring)
        (ring-size ring))))
    (_ thing)))

;;;; Completion

(defun partial-recall--complete-dream (prompt)
  "Complete dream buffer using PROMPT."
  (let* ((predicate (lambda (it) (and-let* ((buffer (cdr it))
                                       ((partial-recall--buffer-mapped-p buffer))
                                       ((not (partial-recall--buffer-in-memory-p buffer)))))))
         (current (current-buffer))
         (initial (unless (partial-recall--buffer-in-memory-p current)
                    (buffer-name current))))

    (partial-recall--complete-buffer prompt predicate initial)))

(defun partial-recall--complete-reality (prompt &optional no-preselect exclude-current)
  "Complete reality buffer using PROMPT.

If NO-PRESELECT is t, no initial input is set.

If EXCLUDE-CURRENT is t, don't include the current buffer."
  (let* ((predicate (lambda (it) (partial-recall--buffer-in-memory-p (cdr it))))
         (current (current-buffer))
         (initial (when (and (not no-preselect) (partial-recall--buffer-in-memory-p current))
                    (buffer-name current))))

    (partial-recall--complete-buffer prompt predicate initial exclude-current)))

(defun partial-recall--complete-removed (prompt)
  "Complete removed buffer using PROMPT."
  (let* ((buffers (partial-recall-memory--removed-buffers))
         (predicate (lambda (it) (and-let* ((buffer (cdr it))
                                       ((member buffer buffers)))))))

    (partial-recall--complete-buffer prompt predicate)))

(defun partial-recall--complete-subconscious (prompt)
  "Complete subconscious buffer using PROMPT."
  (let* ((predicate (lambda (it) (partial-recall--suppressed-p (cdr it))))
         (current (current-buffer))
         (initial (when (partial-recall--suppressed-p current)
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
         (initial (unless (or (partial-recall--buffer-mapped-p current)
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

      (if-let* ((selection (read-buffer prompt initial t predicate))
                (buffer (get-buffer selection)))
          buffer
        (user-error "No buffer selected")))))

(defun partial-recall--complete-memory (prompt)
  "Complete memory using PROMPT."
  (let* ((memories (partial-recall-memories))
         (collection (mapcar (lambda (it) (cons (partial-recall-memory--name it) it)) memories))
         (selection (completing-read prompt collection nil t)))

    (cdr-safe (assoc selection collection))))

;;;; Lighter

(defvar partial-recall-lighter '(" "
                                 (:eval partial-recall-lighter--title)
                                 (partial-recall-lighter-show-info
                                  ("["
                                   (:eval (partial-recall-lighter--moment))
                                   "/"
                                   (:eval (partial-recall-lighter--memory))
                                   "]")))
  "The lighter as a list of mode line constructs.

Shows additional moment and memory info if
`partial-recall-lighter-show-info' is t.")

(put 'partial-recall-lighter 'risky-local-variable t)

(defvar partial-recall-lighter--map
  (let ((map (make-sparse-keymap)))

    (define-key map [mode-line mouse-1] 'partial-recall-lighter--toggle)
    (define-key map [mode-line mouse-3] 'partial-recall-lighter--menu)

    map)
  "Map used in the mode line lighter.")

(defvar partial-recall-lighter--title
  `(:propertize partial-recall-lighter-prefix
                mouse-face mode-line-highlight
                help-echo "Partial Recall\nmouse-1: Toggle permanence\nmouse-3: Menu"
                local-map ,partial-recall-lighter--map)
  "Mode line construct for the lighter title.")

(defun partial-recall-lighter--toggle ()
  "Implant or excise the current buffer."
  (interactive)

  (when-let* ((buffer (current-buffer))
              (moment (partial-recall--find-owning-moment buffer)))

    (partial-recall-make-permanent (current-buffer) (partial-recall-moment--permanence moment))))

(defun partial-recall-lighter--menu ()
  "Show a menu."
  (interactive)

  (let* ((map (make-sparse-keymap))
         (rename (lambda (sym)
                   (substring (symbol-name sym) (1+ (length "partial-recall")))))
         (bind (lambda (_event func)
                 (define-key-after map
                   (vector func)
                   (list 'menu-item (funcall rename func) func)))))

    (define-key-after map [--actions] (list 'menu-item "Partial Recall"))

    (map-keymap bind partial-recall-command-map)

    (condition-case nil
        (popup-menu map)
      (quit nil))))

(defun partial-recall-lighter--moment ()
  "Indicate moment state.

The help echo gives further information."
  (if-let* ((buffer (current-buffer))
            (moment (partial-recall--find-owning-moment buffer)))

      (if (partial-recall--buffer-in-memory-p buffer)
          (partial-recall-lighter--moment-in-memory moment)
        `(:propertize "!"
                      face partial-recall-contrast
                      help-echo "Moment is foreign"))

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

(defun partial-recall-lighter--moment-in-memory (_moment)
  "Get the representation of a MOMENT in memory."
  `(:propertize "-"
                face partial-recall-deemphasized
                help-echo "Moment is fleeting"))

(defun partial-recall-lighter--memory ()
  "Show memory information.

This will normally show the current number of items in the
memory; if the memory has exceeded its original size, the surplus
is shown."
  (and-let* ((memory (partial-recall--reality))
             (ring (partial-recall-memory--moments memory))
             (orig-size (partial-recall-memory--orig-size memory))
             (size (ring-size ring)))

    (if (> size orig-size)

        `(:propertize "+" face partial-recall-emphasis
                      help-echo ,(format "Memory has grown to +%d" (- size orig-size)))

      (let ((face (if (partial-recall-memory--near-capacity-p memory (/ partial-recall-memory-size 2))
                      'partial-recall-contrast
                    'partial-recall-deemphasized))
            (length (ring-length ring)))

        `(:propertize ,(or (partial-recall-graph length size) "-")
                      face ,face
                      help-echo ,(format "Memory contains %d/%d moment(s)" length size))))))

;;;; Setup

(defun partial-recall-mode--setup ()
  "Set up `partial-recall-mode'."
  (unless tab-bar-mode
    (tab-bar-mode 1))

  (partial-recall--queue-tab-fix-up)

  ;; Handle buffer changes.
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
   'view-buffer :before
   #'partial-recall--before-view-buffer)
  (add-hook 'kill-buffer-hook #'partial-recall--forget)

  ;; Handle window configuration changes.
  (advice-add
   'winner-undo :after
   #'partial-recall--after-winner)
  (advice-add
   'winner-redo :after
   #'partial-recall--after-winner)

  ;; Handle tab changes.
  (advice-add
   'tab-bar-switch-to-tab :after
   #'partial-recall--after-tab-bar-switch)
  (advice-add
   'tab-bar-undo-close-tab :before
   #'partial-recall--before-undo-close-tab)
  (advice-add
   'tab-bar-undo-close-tab :after
   #'partial-recall--after-undo-close-tab)
  (add-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (add-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)

  ;; Handle focus changes.
  (add-hook 'minibuffer-setup-hook #'partial-recall--on-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'partial-recall--on-minibuffer-exit)
  (add-hook 'after-make-frame-functions #'partial-recall--queue-tab-fix-up)
  (add-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

(defun partial-recall-mode--teardown ()
  "Tear down `partial-recall-mode'."
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
   'view-buffer
   #'partial-recall--before-view-buffer)
  (remove-hook 'kill-buffer-hook #'partial-recall--forget)

  (advice-remove
   'winner-undo
   #'partial-recall--after-winner)
  (advice-remove
   'winner-redo
   #'partial-recall--after-winner)

  (advice-remove
   'tab-bar-switch-to-tab
   #'partial-recall--after-tab-bar-switch)
  (advice-remove
   'tab-bar-undo-close-tab
   #'partial-recall--before-undo-close-tab)
  (advice-remove
   'tab-bar-undo-close-tab
   #'partial-recall--after-undo-close-tab)
  (remove-hook 'tab-bar-tab-pre-close-functions #'partial-recall--on-close)
  (remove-hook 'tab-bar-tab-post-open-functions #'partial-recall--on-create)

  (remove-hook 'minibuffer-setup-hook #'partial-recall--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'partial-recall--on-minibuffer-exit)
  (remove-hook 'after-make-frame-functions #'partial-recall--queue-tab-fix-up)
  (remove-hook 'delete-frame-functions #'partial-recall--on-frame-delete))

;;;; API

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

  (partial-recall--switch-to-buffer-and-neglect buffer))

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
(defun partial-recall-switch-to-buffer-other-window (buffer &optional arg)
  "Switch to BUFFER in another window.

ARG is passed to `partial-recall--pop-to-buffer-function' as the
second argument (should be NORECORD) The first argument is t."
  (interactive (list (partial-recall--complete-reality "Switch to moment: " t t)
                     current-prefix-arg))

  (let ((pop-up-windows t))

    (funcall partial-recall--pop-to-buffer-function buffer t arg)))

;;;###autoload
(defun partial-recall-reclaim (buffer)
  "Reclaim BUFFER.

The selection is limited to buffers belonging to other memories.
The selected buffer is removed from that memory and subsequently
switched to."
  (interactive (list (partial-recall--complete-dream "Reclaim moment: ")))

  (when-let* ((reclaimed (partial-recall--reclaim buffer t))
              (buffer (partial-recall-moment--buffer reclaimed)))

    (partial-recall--switch-to-buffer-and-neglect buffer)))

;;;###autoload
(defun partial-recall-retrieve (buffer)
  "Retrieve BUFFER.

The selection is limited to buffers that were removed from the
current memory."
  (interactive (list (partial-recall--complete-removed "Retrieve moment: ")))

  (when-let* ((reclaimed (partial-recall--reclaim buffer t))
              (buffer (partial-recall-moment--buffer reclaimed)))

    (partial-recall--switch-to-buffer-and-neglect buffer)))

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

  (partial-recall--forget buffer))

;;;###autoload
(defun partial-recall-banish (buffer)
  "Banish BUFFER.

Removes buffer from the current reality and marks it as no longer
meaningful."
  (interactive (list (partial-recall--complete-reality "Banish moment: ")))

  (partial-recall--forget buffer t))

;;;###autoload
(defun partial-recall-make-permanent (buffer &optional excise)
  "Implant the BUFFER.

This will ensure the buffer is never removed from the memory.

If EXCISE is T, do that instead. The excised buffer can be
removed from the memory again."
  (interactive (list (partial-recall--complete-reality (if current-prefix-arg
                                                           "Excise moment: "
                                                         "Implant moment: "))
                     current-prefix-arg))

  (partial-recall--set-permanence buffer excise))

;;;###autoload
(defun partial-recall-lift (buffer)
  "Lift BUFFER out of the subconscious and switch to it.

Buffers that are forgotten are marked as remnants until they are
remembered again."
  (interactive (list (partial-recall--complete-subconscious "Lift moment: ")))

  (partial-recall--remember buffer)

  (partial-recall--switch-to-buffer-and-neglect buffer))

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

    (partial-recall--switch-to-buffer-and-neglect next)))

;;;###autoload
(defun partial-recall-previous ()
  "Go to the previous moment."
  (interactive)

  (when-let ((previous (partial-recall--previous-buffer)))

    (partial-recall--switch-to-buffer-and-neglect previous)))

;;;###autoload
(defun partial-recall-forget-some ()
  "Prompt the user to forget some moments."
  (interactive)

  (partial-recall--forget-some))

;;;###autoload
(defun partial-recall-remember-some ()
  "Prompt the user to remember some moments."
  (interactive)

  (partial-recall--remember-some))

;;;###autoload
(defun partial-recall-explain-omission ()
  "Explain why the current buffer is non-meaningful."
  (interactive)

  (if-let ((explanation (partial-recall--explain-omission)))
      (message explanation)
    (message "Buffer is meaningful or infringed trait has no explanation")))

;;;###autoload
(defun partial-recall-pop-to-logs ()
  "Switch to the log buffer."
  (interactive)

  (let ((buffer (get-buffer partial-recall-log--buffer-name)))

    (unless buffer
      (user-error "You need to set `partial-recall-log' first"))

    (view-buffer-other-window (get-buffer partial-recall-log--buffer-name))))

(provide 'partial-recall)

;;; partial-recall.el ends here

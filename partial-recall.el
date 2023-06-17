;;; partial-recall.el --- STM for `tab-bar-mode' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Short-term (buffer) memory for `tab-bar-mode' tabs.
;;
;;`partial-recall' will keep track of file buffers opened in a tab in
;; a ring. These rings are called memories in the context of this
;; package; the current memory is called reality, all other memories
;; are dreams. Buffers are time-stamped to (1) allow for the memory to
;; grow if the oldest one is still relatively recent and (2) to allow
;; reclaiming buffers from other tabs if they're relatively old.
;; Buffers are called moments in the context of this package.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)

(defgroup partial-recall nil
  "Short-term (buffer) memory."
  :group 'partial-recall)

(defcustom partial-recall-buffer-limit 20
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
                              (update-count 0))))
  "A moment of partial recall.

A moment is defined by a buffer and a timestamp when that buffer
was remembered."
  buffer timestamp update-count)

(cl-defstruct (partial-recall--memory
               (:constructor partial-recall--memory-create
                             (&aux
                              (ring (make-ring partial-recall-buffer-limit))
                              (orig-size partial-recall-buffer-limit))))
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

(defun partial-recall--memory-at-capacity-p (memory)
  "Check if MEMORY is at capacity."
  (let ((ring (partial-recall--memory-ring memory)))
    (eq (ring-length ring) (ring-size ring))))

(defun partial-recall--reality-moments ()
  "Get the moments from the current memory."
  (when-let ((reality (partial-recall--reality)))

    (partial-recall--memory-ring reality)))

(defun partial-recall--reality-buffer-p (buffer)
  "Check if BUFFER belongs to the current memory."
  (when-let ((moments (partial-recall--reality-moments)))

    (partial-recall--ring-member moments buffer)))

(defun partial-recall--has-buffers-p ()
  "Check if the current memory has buffers."
  (when-let ((moments (partial-recall--reality-moments)))

    (not (ring-empty-p moments))))

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
  (partial-recall--ring-member
   (partial-recall--memory-ring memory)
   buffer))

(defun partial-recall--moment-buffer-p (moment buffer)
  "Check if MOMENT owns BUFFER."
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

(defun partial-recall--should-extend-memory-p (memory)
  "Check if MEMORY should extend its ring size."
  (when-let* ((ring (partial-recall--memory-ring memory))
              (to-remove (ring-ref ring (1- (ring-length ring)))))

    (> partial-recall-max-age
       (- (floor (time-to-seconds))
          (partial-recall--moment-timestamp to-remove)))))

(defun partial-recall--buffer-owner (&optional buffer)
  "Return the memory that owns BUFFER.

Defaults to the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (memories (hash-table-values partial-recall--table)))

    (seq-find (lambda (it) (partial-recall--memory-buffer-p it buffer)) memories)))

(defun partial-recall--reality ()
  "Get the current memory."
  (gethash (partial-recall--key) partial-recall--table))

(defun partial-recall--reality-owns-buffer-p (&optional buffer)
  "Check if reality owns BUFFER."
  (when-let ((memory (partial-recall--reality)))

    (partial-recall--memory-buffer-p memory buffer)))

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

(defun partial-recall--warn (message)
  "Warn about MESSAGE."
  (display-warning 'partial-recall message :warning))

(defun partial-recall--update-count (&optional buffer)
  "Get the udpate count of BUFFER."
  (let* ((buffer (or buffer (current-buffer)))
         (moments (partial-recall--reality-moments))
         (index (partial-recall--ring-member moments buffer))
         (moment (ring-ref moments index)))

    (partial-recall--moment-update-count moment)))

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
  (when-let* ((tab-key (partial-recall--key tab))
              (table partial-recall--table))

    (when (and (not only)
               (gethash tab-key table))
      (remhash tab-key table))))

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
  (when-let* ((tab-key (partial-recall--key))
              (memory (partial-recall--get-or-create-memory tab-key))
              (ring (partial-recall--memory-ring memory)))

    (unless (partial-recall--ring-member ring buffer)
      (when (and (partial-recall--memory-at-capacity-p memory)
                 (partial-recall--should-extend-memory-p memory))
        (ring-extend ring 1))

      (ring-insert ring (partial-recall--moment-create buffer)))))

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
             (moments (partial-recall--reality-moments))
             (index (partial-recall--ring-member moments buffer))
             (moment (ring-ref moments index))
             (count (partial-recall--moment-update-count moment))
             (size (ring-size moments))
             ((or force (>= index (1- size)))))

    (ring-remove+insert+extend moments (ring-ref moments index) t)

    ;; Update timestamp and update count.
    (setf (partial-recall--moment-timestamp moment) (floor (time-to-seconds)))
    (setf (partial-recall--moment-update-count moment) (1+ count))))

(defun partial-recall--reclaim (buffer &optional force)
  "Reclaim BUFFER if possible.

If BUFFER is nil, reclaim the current buffer. If FORCE is t, will
reclaim even if the threshold wasn't passed."
  (and-let* ((reality (partial-recall--reality))
             (owner (partial-recall--buffer-owner buffer))
             ((not (eq reality owner)))
             (ring (partial-recall--memory-ring owner))
             (moment (seq-find (lambda (it) (partial-recall--moment-buffer-p it buffer)) (ring-elements ring)))
             ((or force
                  (< partial-recall-reclaim-min-age
                     (- (floor (time-to-seconds))
                        (partial-recall--moment-timestamp moment)))))
             (index (partial-recall--ring-member ring buffer)))

    ;; Forget in the old memory.
    (ring-remove ring index)

    ;; Remember in the current one.
    (partial-recall--remember buffer)))

(defun partial-recall--forget (&optional buffer)
  "Forget BUFFER."
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
        :require-match t
        :items
        #'(lambda () (consult--buffer-query :sort 'visibility
                                       :predicate #'partial-recall--reality-buffer-p
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
    (define-key map (kbd "s") 'partial-recall-steal)
    (define-key map (kbd "f") 'partial-recall-forget)
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

(defun partial-recall-switch-to-buffer (buffer)
  "Switch to BUFFER."
  (interactive (list (partial-recall--complete-reality "Switch to buffer: ")))

  (switch-to-buffer buffer))

;;;###autoload
(defun partial-recall-reinforce ()
  "Reinforce this buffer."
  (interactive)

  (partial-recall--reinforce (current-buffer) t))

;;;###autoload
(defun partial-recall-reclaim ()
  "Reclaim BUFFER.

This will always force-reclaim."
(partial-recall--reclaim (current-buffer) t))

;;;###autoload
(defun partial-recall-steal (buffer)
  "Steal BUFFER from another memory."
  (interactive (list (partial-recall--complete-dream "Select buffer to steal: ")))

  (partial-recall--reclaim buffer t))

;;;###autoload
(defun partial-recall-forget ()
  "Forget current buffer."
  (interactive)

  (partial-recall--forget (current-buffer)))

(provide 'partial-recall)

;;; partial-recall.el ends here

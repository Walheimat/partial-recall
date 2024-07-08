;;; partial-recall-hygiene.el --- Automatic flushing -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.11.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Adds hygiene behavior to memories. This means that (1) memories are
;; regularly flushed and that (2) a warning is emitted for memories
;; that have reached maximum size.

;;; Code:

(require 'partial-recall)

;;;; Customization

(defcustom partial-recall-hygiene-idle-delay 5
  "Idle delay before running hygiene routine."
  :type 'integer
  :group 'partial-recall)

(defcustom partial-recall-hygiene-flush t
  "Whether to flush memories regularly."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-hygiene-warn-on-full t
  "Whether a memory reaching maximum size should emit a warning.

This is only done once."
  :type 'boolean
  :group 'partial-recall)

(defcustom partial-recall-hygiene-nag-buffer-action-params '((window-height . 1))
  "The buffer display action alist for nag buffers."
  :type 'alist
  :group 'partial-recall)

(defcustom partial-recall-hygiene-nag-buffer-action 'display-buffer-at-bottom
  "Action used to display nag buffers."
  :type 'symbol
  :group 'partial-recall)

;;;; Private variables

(defvar partial-recall-hygiene--timer nil
  "Timer for for `partial-recall-hygiene-mode'.

Runs `partial-recall-hygiene--on-idle'.")

(defvar partial-recall-hygiene--warned nil
  "List of memories that have already.")

(defvar partial-recall-hygiene--nag-buffer-name "*partial-recall-nag*"
  "Name of the buffer displaying nags.")

;;;; Functionality

(defun partial-recall-hygiene--minibuffer-active-p ()
  "Check if the minibuffer is active."
  (seq-find #'window-minibuffer-p (window-list-1)))

(defun partial-recall-hygiene--nag (msg)
  "Nag user about MSG."
  (let ((buffer (get-buffer-create partial-recall-hygiene--nag-buffer-name))
        (inhibit-read-only t))

    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert msg)

      (read-only-mode))

    (let ((window (display-buffer
                   buffer
                   (cons partial-recall-hygiene-nag-buffer-action
                         partial-recall-hygiene-nag-buffer-action-params))))

      (unless (partial-recall-hygiene--minibuffer-active-p)
        (select-window window)))))

(defun partial-recall-hygiene--on-idle ()
  "Run a hygiene routine.

This flushes moments from all memories.

See `partial-recall--flush' and `partial-recall-memorable-traits' for
the heuristics on which moments get flushed."
  (partial-recall-log "Running hygiene routine")

  (let ((sum 0)
        (full nil))
    (dolist (memory (partial-recall-memories))

      ;; Flush moments.
      (when partial-recall-hygiene-flush
        (setq sum (+ sum
                     (partial-recall--flush memory))))

      ;; Keep track of full memories.
      (when (and partial-recall-hygiene-warn-on-full
                 (partial-recall-memory--near-capacity-p memory)

                 (not (member (partial-recall-memory--unique memory) partial-recall-hygiene--warned)))

        (setq full (append full (list (partial-recall-memory--unique memory))))))

    (when (> sum 0)
      (partial-recall-log "Flushed %d moments in total" sum))

    (when full
      (setq partial-recall-hygiene--warned full)

      (partial-recall-hygiene--nag
       (format "Following memories are full: %s"
               (mapconcat #'identity full ", "))))))

;;;; API

;;;###autoload
(define-minor-mode partial-recall-hygiene-mode
  "Adds automatic flushing to `partial-recall' memories."
  :group 'partial-recall
  :global t
  (if partial-recall-hygiene-mode

      (setq partial-recall-hygiene--timer
            (run-with-idle-timer partial-recall-hygiene-idle-delay t #'partial-recall-hygiene--on-idle))

    (cancel-timer partial-recall-hygiene--timer)
    (setq partial-recall-hygiene--timer nil)))

(provide 'partial-recall-hygiene)

;;; partial-recall-hygiene.el ends here

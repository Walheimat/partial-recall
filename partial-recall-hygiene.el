;;; partial-recall-hygiene.el --- Automatic flushing -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.11.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Adds hygiene behavior to memories. This means that memories are
;; regularly flushed.

;;; Code:

(require 'partial-recall)

;;;; Customization

(defcustom partial-recall-hygiene-idle-delay 5
  "Idle delay before flushing moments."
  :type 'integer
  :group 'partial-recall)

;;;; Private variables

(defvar partial-recall-hygiene--timer nil
  "Timer for for `partial-recall-hygiene-mode'.

Runs `partial-recall-hygiene--on-idle'.")

;;;; Functionality

(defun partial-recall-hygiene--on-idle ()
  "Run a hygiene routine.

This flushes moments from all memories.

See `partial-recall--flush' and `partial-recall-memorable-traits' for
the heuristics on which moments get flushed."
  (partial-recall-log "Running hygiene routine")

  (let ((sum 0))
    (dolist (memory (partial-recall-memories))

      (setq sum (+ sum
                   (partial-recall--flush memory))))

    (when (> sum 0)
      (partial-recall-log "Flushed %d moments in total" sum))))

;;;; API

;;;###autoload
(define-minor-mode partial-recall-hygiene-mode
  "Adds automatic flushing to `partial-recall' memories."
  :group 'partial-recall
  :global t
  (if partial-recall-hygiene-mode

      (setq partial-recall-hygiene--timer
            (run-with-idle-timer 2 partial-recall-hygiene-idle-delay #'partial-recall-hygiene--on-idle))

    (cancel-timer partial-recall-hygiene--timer)
    (setq partial-recall-hygiene--timer nil)))

(provide 'partial-recall-hygiene)

;;; partial-recall-hygiene.el ends here

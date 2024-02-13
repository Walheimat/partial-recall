;;; partial-recall-concentration.el --- Focus through concentration -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.10.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; This is an optional behavior that increases a moment's focus when
;; it remains visible over long periods of time.

;;; Code:

(require 'partial-recall)

;;; Customization

(defcustom partial-recall-concentration-cycle 60
  "Number of seconds a cycle of concentration takes.

If a buffer remains visible from when the cycle began until it
ends, its focus is increased. Can be set to nil to disable
concentration."
  :type '(choice (integer :tag "Number of seconds")
                 (const :tag "Don't use" nil))
  :group 'partial-recall)

;;; Variables

(defvar partial-recall-concentration--timer nil)
(defvar partial-recall-concentration--deferred nil)

;;; Errors

(define-error 'prcon-not-owned "Buffer is not owned" 'partial-recall-errors)
(define-error 'prcon-changed "Buffer has changed" 'partial-recall-errors)
(define-error 'prcon-not-ready "No reality" 'partial-recall-errors)

(defun partial-recall-concentration--concentrate ()
  "Concentrate on the current moment.

If the moment has remained the same since the last cycle, its
focus is intensified, otherwise concentration breaks and the now
current moment is focused."
  (condition-case err
      (progn
        (partial-recall-concentration--hold)

        ;; Move faint to last focus if it exists.
        (when partial-recall--faint-focus
          (setq partial-recall--last-focus partial-recall--faint-focus
                partial-recall--faint-focus nil)
          (partial-recall-concentration--renew))

        (partial-recall-moment--intensify partial-recall--last-focus nil 'concentrate))

    ((prcon-not-ready prcon-not-owned)
     (partial-recall-concentration--defer))
    (prcon-changed
     (let ((moment (cdr err)))

       (when-let ((lost (or partial-recall--faint-focus partial-recall--last-focus)))
         (partial-recall-debug "Concentration on `%s' broke" lost))

       (partial-recall-concentration--renew)
       (partial-recall-debug "Concentration on `%s' begins" moment)

       (setq partial-recall--last-focus moment
             partial-recall--faint-focus nil)))))

(defun partial-recall-concentration--hold ()
  "Try to hold concentration."
  (unless (partial-recall--reality)
    (signal 'prcon-not-ready nil))

  (let* ((buffer (current-buffer))
         (moment (partial-recall--find-owning-moment buffer))
         (focus (or partial-recall--last-focus partial-recall--faint-focus)))

    (unless (or moment (partial-recall--buffer-in-memory-p buffer))
      (signal 'prcon-not-owned buffer))

    (unless (and focus
                 (or (eq moment focus)
                     (partial-recall--buffer-visible-p
                      (partial-recall-moment--buffer focus))))

      (signal 'prcon-changed moment))

    (partial-recall-debug "Concentration held on `%s'" focus)))

(defun partial-recall-concentration--defer ()
  "Defer concentration.

This restarts the cycle with a much shorter repeat time until
concentration on a moment can begin. The last focus is retained
as a faint focus.

If deferring is already in place, the faint focus is lost as
well."
  (if partial-recall-concentration--deferred
      (setq partial-recall--faint-focus nil)

    (partial-recall-debug "Deferring concentration")

    (setq partial-recall-concentration--deferred t
          partial-recall--faint-focus partial-recall--last-focus
          partial-recall--last-focus nil)

    (partial-recall-concentration--start nil (/ partial-recall-concentration-cycle 10))))

(defun partial-recall-concentration--renew ()
  "Renew concentration cycle."
  (when partial-recall-concentration--deferred
    (partial-recall-debug "Aborting deferred concentration")

    (setq partial-recall-concentration--deferred nil)

    (partial-recall-concentration--start partial-recall-concentration-cycle)))

(defun partial-recall-concentration--shift (name)
  "Re-concentrate after switching to NAME.

This cancels and re-runs the timer."
  (partial-recall-debug "Shifting concentration towards `%s'" name)

  (partial-recall-concentration--start))

(defun partial-recall-concentration--start (&optional secs repeat)
  "Start concentrating.

This cancels a previously running timer.

Optionally SECS and REPEAT can be passed which are passed along
to `run-with-timer'. They default to
`partial-recall-handle-delay' and
`partial-recall-concentration-cycle'."
  (when partial-recall-concentration--timer
    (cancel-timer partial-recall-concentration--timer))

  (when-let ((secs (or secs partial-recall-handle-delay))
             (repeat (or repeat partial-recall-concentration-cycle)))

    (setq partial-recall-concentration--timer (run-with-timer
                                               (1+ secs)
                                               (1+ repeat)
                                               #'partial-recall-concentration--concentrate))))

;;;; Mode

(defun partial-recall-concentration--setup ()
  "Setup `partial-recall-concentration-mode'."
  (when (partial-recall--reality)
    (partial-recall-concentration--concentrate))

  (add-hook
   'partial-recall-after-create-hook
   #'partial-recall-concentration--shift)

  (advice-add
   'tab-bar-switch-to-tab :after
   #'partial-recall-concentration--shift))

(defun partial-recall-concentration--teardown ()
  "Tear down `partial-recall-concentration-mode'."
  (cancel-timer partial-recall-concentration--timer)

  (remove-hook
   'partial-recall-after-create-hook
   #'partial-recall-concentration--shift)

  (advice-remove
   'tab-bar-switch-to-tab
   #'partial-recall-concentration--shift))

;;;###autoload
(define-minor-mode partial-recall-concentration-mode
  "Mode that increases a moment's focus through concentration."
  :group 'partial-recall
  :global t
  (if partial-recall-concentration-mode
      (partial-recall-concentration--setup)
    (partial-recall-concentration--teardown)))

(provide 'partial-recall-concentration)

;;; partial-recall-concentration.el ends here

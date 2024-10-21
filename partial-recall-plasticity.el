;;; partial-recall-plasticity.el --- Dynamically sized memories -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.13.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Adds plasticity behavior to memories. Memories can now dynamically
;; grow and shrink.
;;
;; Adds plasticity behavior to moment. Moments are made automatically
;; permanent and permanent moments no longer leave a memory.

;;; Code:

(require 'partial-recall)
(require 'partial-recall-menu)

;;;; Customization

(defcustom partial-recall-plasticity-implant-threshold 100
  "The amount of focus before auto-implanting a moment.

If this is nil, never auto-implant."
  :type '(integer :tag "Minimum focus before auto-implanting")
  :group 'partial-recall)

;;;; Memory

(defun partial-recall-plasticity--resize-or-extend-memory (memory)
  "Maybe resize MEMORY."
  (or  (partial-recall-plasticity--maybe-resize-memory memory)
       (partial-recall-plasticity--maybe-extend-memory memory)))

(defun partial-recall-plasticity--maybe-resize-memory (memory)
  "Maybe resize MEMORY if it has grown but could shrink."
  (let ((ring (partial-recall-memory--moments memory))
        (orig (partial-recall-memory--orig-size memory))
        (curr (ring-size (partial-recall-memory--moments memory))))

    (when (and (not (partial-recall-memory--near-capacity-p memory))
               (> curr orig))

      (partial-recall-debug "Resizing `%s'" memory)

      (ring-resize ring (max (ring-length ring) orig)))))

(defun partial-recall-plasticity--maybe-extend-memory (memory)
  "Maybe extend MEMORY.

See `partial-recall-plasticity--should-extend-p'."
  (when (and (partial-recall-memory--near-capacity-p memory)
             (partial-recall-plasticity--should-extend-p memory))

    (partial-recall-debug "Extending `%s'" memory)

    (ring-extend (partial-recall-memory--moments memory) 1)))

(defun partial-recall-plasticity--should-extend-p (memory)
  "Check if MEMORY should extend its ring size.

This is the case if the oldest ring element is still a short-term
moment."
  (and-let* ((ring (partial-recall-memory--moments memory))
             (to-remove (partial-recall--ring-oldest ring))
             (youngest (partial-recall--ring-youngest ring)))

    (or (partial-recall--short-term-p to-remove)
        (partial-recall-plasticity--free-recall youngest))))

(defun partial-recall-plasticity--free-recall (moment)
  "Check if MOMENT is very recent."
  (and-let* (((numberp partial-recall-intermediate-term))
             (half-term (/ partial-recall-intermediate-term 2)))
    (partial-recall--falls-below-p moment half-term)))

;;;; Moment

(defun partial-recall-plasticity--maybe-implant-or-excise (moment count)
  "Check if MOMENT should be implanted automatically.

This is true if COUNT exceeds
`partial-recall-platicity-implant-threshold'.

If MOMENT already is implanted but no longer has the necessary focus,
this will excise it without resetting the count."
  (cond
   ((and (not (partial-recall-moment--permanence moment))
         (>= count partial-recall-plasticity-implant-threshold))
    (partial-recall-debug "Focus on `%s' raised to auto-implant threshold" moment)

    (partial-recall--set-permanence (partial-recall-moment--buffer moment)))

   ((and (partial-recall-moment--permanence moment)
         (< count partial-recall-plasticity-implant-threshold))
    (partial-recall-debug "Focus on `%s' lowered below auto-implant threshold" moment)

    (partial-recall--set-permanence (partial-recall-moment--buffer moment) t t))))

(defun partial-recall-plasticity--maybe-reinsert-implanted (memory)
  "Maybe reinforce oldest moments in MEMORY.

This will loop over the moments in reverse and makes sure to
re-insert any implanted one."
  (and-let* ((ring (partial-recall-memory--moments memory))
             (oldest (partial-recall--ring-oldest ring)))

    (let ((checked nil))

      (while (and oldest
                  (not (memq oldest checked))
                  (partial-recall-moment--permanence oldest))
        (partial-recall--reinsert oldest memory "permanent moment")
        (push oldest checked)
        (setq oldest (partial-recall--ring-oldest ring))))))

(defun partial-recall-plasticity--gracedp (moment &rest _)
  "Check if MOMENT was graced.

This holds true for permanent moments or those whose focus is at
least half of the implant threshold."
  (or (partial-recall-moment--permanence moment)
      (>= (partial-recall-moment--focus moment)
          (/ partial-recall-plasticity-implant-threshold 2))))

(defun partial-recall-plasticity--print-presence (focus implanted)
  "Format presence using FOCUS.

If the moment is IMPLANTED, signal that."
  (let* ((text (or (partial-recall-graph focus partial-recall-plasticity-implant-threshold)
                   (if implanted
                       partial-recall-menu--persistence-indicator
                     partial-recall-menu--empty)))
         (face (if implanted 'partial-recall-soothe 'partial-recall-deemphasized))
         (help (format "Focus: %s, Implanted: %s" focus implanted)))

    (propertize text 'face face 'help-echo help)))

(defun partial-recall-plasticity--moment-in-memory (moment)
  "Get the lighter segment for MOMENT."
  (if (partial-recall-moment--permanence moment)
      `(:propertize "*"
                    face partial-recall-contrast
                    help-echo ,(concat "Moment is implanted"))
    (when-let* ((focus (partial-recall-moment--focus moment))
                (perc (* 100 (/ focus (* 1.0 partial-recall-plasticity-implant-threshold))))
                (graph (partial-recall-graph focus partial-recall-plasticity-implant-threshold)))
      `(:propertize ,graph
                    face partial-recall-deemphasized
                    help-echo ,(format "Moment is fleeting (focus %d%%)" perc)))))

;;;; Modes

;;;###autoload
(define-minor-mode partial-recall-plasticity-of-memory-mode
  "Add plasticity behavior to `partial-recall' memories."
  :group 'partial-recall
  :global t
  (if partial-recall-plasticity-of-memory-mode
      (add-hook
       'partial-recall-before-probe-hook
       #'partial-recall-plasticity--resize-or-extend-memory)
    (remove-hook
     'partial-recall-before-probe-hook
     #'partial-recall-plasticity--resize-or-extend-memory)))

;;;###autoload
(define-minor-mode partial-recall-plasticity-of-moment-mode
  "Add plasticity behavior to `partial-recall' moments."
  :group 'partial-recall
  :global t
  (if partial-recall-plasticity-of-moment-mode
      (progn
        (add-hook
         'partial-recall-before-probe-hook
         #'partial-recall-plasticity--maybe-reinsert-implanted)

        (add-hook
         'partial-recall-after-focus-change-hook
         #'partial-recall-plasticity--maybe-implant-or-excise)

        (advice-add
         #'partial-recall-lighter--moment-in-memory :before-until
         #'partial-recall-plasticity--moment-in-memory)

        (advice-add
         #'partial-recall--gracedp :before-until
         #'partial-recall-plasticity--gracedp)

        ;; Menu:
        (advice-add
         #'partial-recall-menu--print-presence :override
         #'partial-recall-plasticity--print-presence))

    (remove-hook
     'partial-recall-before-probe-hook
     #'partial-recall-plasticity--maybe-reinsert-implanted)

    (remove-hook
     'partial-recall-after-focus-change-hook
     #'partial-recall-plasticity--maybe-implant-or-excise)

    (advice-remove
     #'partial-recall-lighter--moment-in-memory
     #'partial-recall-plasticity--moment-in-memory)

    (advice-remove
     #'partial-recall--gracedp
     #'partial-recall-plasticity--gracedp)

    (advice-remove
     #'partial-recall-menu--print-presence
     #'partial-recall-plasticity--print-presence)))

(provide 'partial-recall-plasticity)

;;; partial-recall-plasticity.el ends here

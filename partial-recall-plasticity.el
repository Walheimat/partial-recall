;;; partial-recall-plasticity.el --- Dynamically sized memories -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.10.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: frames files convenience

;;; Commentary:

;; Adds plasticity behavior to memories. Memories can now dynamically
;; grow and shrink.

;;; Code:

(require 'partial-recall)

(defun partial-recall-plasticity--maybe-resize (memory)
  "Maybe resize MEMORY."
  (or  (partial-recall-plasticity--maybe-resize-memory memory)
       (partial-recall-plasticity--maybe-extend-memory memory)))

(defun partial-recall-plasticity--maybe-resize-memory (memory)
  "Maybe resize MEMORY if it has grown but could shrink."
  (let ((ring (partial-recall-memory--moments memory))
        (orig (partial-recall-memory--orig-size memory))
        (curr (ring-size (partial-recall-memory--moments memory))))

    (when (and (not (partial-recall-memory--at-capacity-p memory))
               (> curr orig))

      (partial-recall-debug "Resizing `%s'" memory)

      (ring-resize ring (max (ring-length ring) orig)))))

(defun partial-recall-plasticity--maybe-extend-memory (memory)
  "Maybe extend MEMORY.

See `partial-recall-plasticity--should-extend-p'."
  (when (and (partial-recall-memory--at-capacity-p memory)
             (partial-recall-plasticity--should-extend-p memory))

    (partial-recall-debug "Extending `%s'" memory)

    (ring-extend (partial-recall-memory--moments memory) 1)))

(defun partial-recall-plasticity--should-extend-p (memory)
  "Check if MEMORY should extend its ring size.

This is the case if the oldest ring element is still a short-term
moment."
  (and-let* ((ring (partial-recall-memory--moments memory))
             (to-remove (partial-recall--ring-oldest ring))
             ((partial-recall--short-term-p to-remove)))))

;;;; Mode

(define-minor-mode partial-recall-plasticity-mode
  "Add plasticity behavior to `partial-recall' memories."
  :group 'partial-recall
  :global t
  (if partial-recall-plasticity-mode
      (add-hook
       'partial-recall-before-probe-hook
       #'partial-recall-plasticity--maybe-resize)
    (remove-hook
     'partial-recall-before-probe-hook
     #'partial-recall-plasticity--maybe-resize)))

(provide 'partial-recall-plasticity)


;;; partial-recall-plasticity.el ends here

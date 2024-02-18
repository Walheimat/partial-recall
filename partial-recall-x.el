;;; partial-recall-x.el --- Extension for `partial-recall' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.10.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Functionality to integrate `partial-recall' in external libraries
;; or make `partial-recall' accessible from external code.

;;; Code:

(declare-function consult--buffer-state "ext:consult.el")
(declare-function consult--buffer-query "ext:consult.el")

(require 'partial-recall)

;;;###autoload
(defvar partial-recall-x-consult-buffer-source
  (list :name "Partial Recall"
        :hidden t
        :narrow ?u
        :category 'buffer
        :state #'consult--buffer-state
        :history 'buffer-name-history
        :require-match t
        :items
        #'(lambda () (consult--buffer-query :sort 'visibility
                                       :predicate #'partial-recall--buffer-in-memory-p
                                       :as #'buffer-name)))
  "Buffers that are recalled from the current tab.")

;;;###autoload
(defun partial-recall-x-buffer-specs (&optional buffer)
  "Get the BUFFER's specs.

The specs are a plist of the attributes `:meaningful', `:real'
and `:implanted'."
  (interactive)

  (let* ((buffer (or buffer (current-buffer)))
         (specs (list :meaningful (partial-recall--meaningful-buffer-p buffer)
                      :real (not (null (partial-recall--buffer-in-memory-p buffer)))
                      :implanted (buffer-local-value 'partial-recall--permanent buffer))))

    (if (called-interactively-p 'any)
        (let ((partial-recall-log t))
          (partial-recall-log "Buffer '%s' has specs '%s'" buffer specs))
      specs)))

(defun partial-recall-x-memory-specs (&optional memory)
  "Get the MEMORY's specs.

The specs are a plist of attributes `:size' and `:capacity' and
`:original-capacity'."
  (interactive)

  (let* ((memory (or memory (partial-recall--reality)))
         (specs (list :size (ring-length (partial-recall-memory--moments memory))
                      :capacity (ring-size (partial-recall-memory--moments memory))
                      :original-capacity (partial-recall-memory--orig-size memory))))

    (if (called-interactively-p 'any)
        (let ((partial-recall-log t))
          (partial-recall-log "Memory '%s' has specs '%s'" memory specs))
      specs)))

(provide 'partial-recall-x)

;;; partial-recall-x.el ends here

;;; partial-recall-extensions.el --- Extension for `partial-recall' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>

;;; Commentary:

;; Extension to integrate `partial-recall' in external libraries.

;;; Code:

(declare-function consult--buffer-state "ext:consult.el")
(declare-function consult--buffer-query "ext:consult.el")

(require 'partial-recall)

;;;###autoload
(defvar partial-recall--consult-buffer-source
  (list :name "Partial Recall"
        :narrow ?r
        :category 'buffer
        :state #'consult--buffer-state
        :history 'buffer-name-history
        :require-match t
        :items
        #'(lambda () (consult--buffer-query :sort 'visibility
                                       :predicate #'partial-recall--memory-buffer-p
                                       :as #'buffer-name)))
  "Buffers that are recalled from the current tab.")

(provide 'partial-recall-extensions)

;;; partial-recall-extensions.el ends here

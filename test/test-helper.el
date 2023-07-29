;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'bydi-ci)
(require 'bydi-report)

;; Helpers

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))

(cl-defmacro with-tab-history (&rest body &key pre lifts second &allow-other-keys)
  "Run BODY with a clear tab history and a temp buffer.

If PRE is t, pre-remember the current buffer. Unless LIFTS is t,
ignore calls to `partial-recall--lift'. If SECOND is true, a
second memory is created."
  (declare (indent defun))
  `(bydi ((:mock tab-bar--current-tab :return test-tab)
          (:mock tab-bar-tabs :return (list test-tab))
          (:always partial-recall--meaningful-buffer-p)
          ,(unless lifts '(:ignore partial-recall--lift)))
     (let* ((partial-recall--table (make-hash-table))
            (second-memory-key "second")
            (second-memory nil))

       (with-temp-buffer
         ,(when pre '(partial-recall--remember (current-buffer)))
         ,(when second
            '(progn
               (setq second-memory (partial-recall--memory-create second-memory-key))
               (puthash second-memory-key second-memory partial-recall--table)))
         ,@body))))

;; Setup

(bydi-ci-setup-paths)
(bydi-report-setup-undercover (list "*.el"))
(bydi-report-setup-ert-runner)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

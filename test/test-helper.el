;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'bydi-ci)
(require 'bydi-report)

(setq byte-compile-warnings '(not not-unused))

;; Helpers

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))
(defvar test-table (make-hash-table :test #'equal))
(defvar test-tabs (list test-tab))

(cl-defmacro with-tab-history (&rest body &key pre lifts probes second wavers &allow-other-keys)
  "Execute BODY in a clean environment.

This environment is a clear tab history and a single existing
static tab.

If PRE is t, pre-remember the current buffer. Unless LIFTS is t,
ignore calls to `partial-recall--lift'. Unless PROBES is t,
ignore calls to `partial-recall--probe-memory'. If SECOND is
true, a second memory is created. If WAVERS is t,
`partial-recall--meaningful-buffer-p' is not always t."
  (declare (indent defun))

  `(bydi ((:mock tab-bar--current-tab :return test-tab)
          (:mock tab-bar-tabs :return test-tabs)
          ,(unless probes 'partial-recall--probe-memory)
          ,(unless wavers '(:always partial-recall--meaningful-buffer-p))
          ,(unless lifts '(:ignore partial-recall--lift)))

     (let ((partial-recall--table test-table)
           (second-memory-key "second")
           (second-memory nil))

       ,(when pre '(setq partial-recall--last-focus
                         (partial-recall--remember (current-buffer))))

       ,(when second
          '(progn
             (setq second-memory (partial-recall--memory-create second-memory-key))
             (puthash second-memory-key second-memory partial-recall--table)))

       (unwind-protect
           ,@body
         (clrhash test-table)))))

;; Setup

(bydi-ci-setup-paths)
(bydi-report-setup-undercover (list "*.el"))
(bydi-report-setup-ert-runner)
(bydi-report-setup-ert :increase-print-depth t)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

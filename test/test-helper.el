;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'bydi-report)

(declare-function bydi-path-setup "ext:bydi.el")
(declare-function bydi-ert-runner-setup "ext:bydi.el")
(declare-function bydi-undercover-setup "ext:bydi.el")

;; Helpers

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))

(cl-defmacro with-tab-history (&rest body &key pre lifts &allow-other-keys)
  "Run BODY with a clear tab history and a temp buffer.

If PRE is t, pre-remember the current buffer. Unless LIFTS is t,
ignore calls to `partial-recall--lift'."
  (declare (indent defun))
  `(bydi ((:mock tab-bar--current-tab :return test-tab)
          (:mock tab-bar-tabs :return (list test-tab))
          (:always partial-recall--meaningful-buffer-p)
          ,(unless lifts '(:ignore partial-recall--lifted)))
     (let ((partial-recall--table (make-hash-table)))

       (with-temp-buffer
         ,(when pre '(partial-recall--remember (current-buffer)))
         ,@body))))

;; Setup

(bydi-path-setup)
(bydi-undercover-setup (list "*.el"))
(bydi-ert-runner-setup)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

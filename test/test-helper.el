;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'dinghy-rope)

(setq byte-compile-warnings '(not not-unused))

;; Helpers

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))
(defvar test-table (make-hash-table :test #'equal))
(defvar test-tabs (list test-tab))

(cl-defmacro with-tab-history ((&key pre lifts probes second-mem second-mom wavers) &rest body)
  "Execute BODY in a clean environment.

This environment is a clear tab history and a single existing
static tab.

If PRE is t, pre-remember the current buffer. Unless LIFTS is t,
ignore calls to `partial-recall--lift'. Unless PROBES is t,
ignore calls to `partial-recall--probe-memory'. If SECOND-MEM is
t, a second memory is created. If SECOND-MOM is t, a second
moment is created. If WAVERS is t,
`partial-recall--meaningful-buffer-p' is not always t."
  (declare (indent defun))

  `(bydi ((:mock tab-bar--current-tab :return test-tab)
          (:mock tab-bar-tabs :return test-tabs)
          ,@(delq nil
                  `(,(unless probes 'partial-recall--probe-memory)
                    ,(unless wavers '(:always partial-recall--meaningful-buffer-p))
                    ,(unless lifts '(:ignore partial-recall--lift)))))

     (let ((partial-recall--table test-table)
           (second-memory-key "second")
           (second-memory nil)
           (second-moment nil)
           (second-moment-buffer nil))

       ,@(delq
          nil
          `(,(when pre
               '(setq partial-recall--last-focus
                      (partial-recall--remember (current-buffer))))

            ,(when second-mom
               (setq second-moment-buffer (generate-new-buffer " *temp*" t))
               (setq second-moment (partial-recall--remember second-moment-buffer)))

            ,(when second-mem
               '(progn
                  (setq second-memory (partial-recall-memory--create second-memory-key))
                  (puthash second-memory-key second-memory partial-recall--table)))))

       (unwind-protect
           ,@body
         (clrhash test-table)
         (when (and second-moment-buffer (buffer-live-p second-moment-buffer))
           (kill-buffer second-moment-buffer))))))

;; Setup

(dinghy-rope-setup-paths)
(dinghy-rope-setup-undercover (list "*.el"))
(dinghy-rope-setup-ert-runner)
(dinghy-rope-setup-ert :increase-print-depth t)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

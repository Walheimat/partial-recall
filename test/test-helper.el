;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'ert-x)
(require 'compat nil t)
(require 'undercover nil t)

(defvar mock-history nil)

(defmacro with-mock (to-mock &rest body)
  "Evaluate BODY mocking list of function(s) TO-MOCK.

TO-MOCK maybe be a single item or a list of items.

The arguments passed to the mocked functions will be recorded in
a hash table. Repeated calls will append results.

Each item in TO-MOCK can either be a function symbol or a cons
cell of shape (FUNCTION . MOCK-IMPLEMENTATION). The return value
is either the argument list or the result of the mock
implementation."
  (declare (indent defun))

  `(cl-letf* ((mock-history (make-hash-table :test 'equal))
              (remember (lambda (fun args)
                          (let* ((prev (gethash fun mock-history))
                                 (val (if prev (push args prev) (list args))))
                            (puthash fun val mock-history)
                            args)))
              ,@(mapcar (lambda (it)
                          (cond
                           ((consp it)
                            `((symbol-function ',(car it))
                              (lambda (&rest r)
                                (interactive)
                                (apply remember (list ',(car it) r))
                                (apply ,(cdr it) r))))
                           (t
                            `((symbol-function ',it)
                              (lambda (&rest r)
                                (interactive)
                                (apply remember (list ',it r)))))))
                        (if (listp to-mock) to-mock (list to-mock))))
     ,@body))

(defun wal-clear-mocks ()
  "Clear mock history."
  (setq mock-history (make-hash-table :test 'equal)))

(defmacro was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (car (gethash ',fun mock-history))))))

(defmacro was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (nth ,index (reverse (gethash ',fun mock-history)))))))

(defmacro was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun mock-history 'not-called)))
     (should-not (equal 'not-called actual))))

(defmacro was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun mock-history 'not-called)))
     (should (equal 'not-called actual))))

(defmacro was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(should (equal ,expected (length (gethash ',fun mock-history)))))

(defun test-helper--undercover-setup ()
  "Set up `undercover'."
  (when (featurep 'undercover)
    (message "Setting up `undercover'")

    (let ((report-format 'text)
          (report-file "./coverage/results.txt"))

      (setq undercover-force-coverage t)

      (cond
       ((getenv "CI")
        (setq report-format 'lcov
              report-file nil))

       ((getenv "COVERAGE_WITH_JSON")
        (setq undercover--merge-report nil
              report-format 'simplecov
              report-file "./coverage/.resultset.json")))

      (undercover--setup
       (list "partial-recall.el"
             (list :report-format report-format)
             (list :report-file report-file)
             (list :send-report nil))))))

(defun test-helper--report (&rest _)
  "Report the coverage."
  (unless (or (getenv "CI")
              (getenv "COVERAGE_WITH_JSON"))

    (let ((coverage "./coverage/results.txt"))

      (when (file-exists-p coverage)
        (with-temp-buffer
          (insert-file-contents-literally coverage)

          (let ((contents (buffer-string)))

            (message "\n%s" contents)))))))

;; Setup

(test-helper--undercover-setup)

(let* ((source-dir (expand-file-name (or (getenv "GITHUB_WORKSPACE")
                                         default-directory))))

    (message "Setting source path to %s" source-dir)

    (add-to-list 'load-path source-dir))

(add-hook
 'ert-runner-reporter-run-ended-functions
 #'test-helper--report)

;;; test-helper.el ends here

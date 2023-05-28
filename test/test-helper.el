;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'ert-x)
(require 'compat nil t)
(require 'undercover nil t)

(defvar wal-mock-history nil)

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

  `(cl-letf* ((wal-mock-history (make-hash-table :test 'equal))
              (remember (lambda (fun args)
                          (let* ((prev (gethash fun wal-mock-history))
                                 (val (if prev (push args prev) (list args))))
                            (puthash fun val wal-mock-history)
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
  (setq wal-mock-history (make-hash-table :test 'equal)))

(defmacro was-called-with (fun expected)
  "Check if FUN was called with EXPECTED."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (car (gethash ',fun wal-mock-history))))))

(defmacro was-called-nth-with (fun expected index)
  "Check if FUN was called with EXPECTED on the INDEXth call."
  (let ((safe-exp (if (listp expected) expected `(list ,expected))))
    `(should (equal ,safe-exp (nth ,index (reverse (gethash ',fun wal-mock-history)))))))

(defmacro was-called (fun)
  "Check if mocked FUN was called."
  `(let ((actual (gethash ',fun wal-mock-history 'not-called)))
     (should-not (equal 'not-called actual))))

(defmacro was-not-called (fun)
  "Check if mocked FUN was not called."
  `(let ((actual (gethash ',fun wal-mock-history 'not-called)))
     (should (equal 'not-called actual))))

(defmacro was-called-n-times (fun expected)
  "Check if mocked FUN was called EXPECTED times."
  `(should (equal ,expected (length (gethash ',fun wal-mock-history)))))

;;; test-helper.el ends here

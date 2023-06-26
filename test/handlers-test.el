;;; handlers-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

;; Handlers

(ert-deftest pr--handle-buffer ()
  (with-tab-history
    (bydi (partial-recall--remember
           (:always buffer-live-p)
           (:ignore partial-recall--mapped-buffer-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--remember))))

(ert-deftest pr--handle-buffer--reclaims ()
  (with-tab-history
    (bydi (partial-recall--remember
           partial-recall--recollect
           (:always buffer-live-p)
           (:always partial-recall--mapped-buffer-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--recollect))))

(ert-deftest pr--on-create--sets-cdr ()
  (bydi ((:mock partial-recall--create-key :return "test"))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab))))))

(ert-deftest pr--on-close ()
  (bydi (partial-recall--suppress)
    (with-tab-history :pre t

      (should (eq 1 (length (hash-table-keys partial-recall--table))))

      (partial-recall--on-close test-tab t)

      (should (eq 1 (length (hash-table-keys partial-recall--table))))

      (partial-recall--on-close test-tab nil)

      (should (eq 0 (length (hash-table-keys partial-recall--table))))

      (bydi-was-called partial-recall--suppress))))

(ert-deftest pr--on-buffer-list-update--cancels-running-timer ()
  (let ((partial-recall--timer nil))

    (bydi ((:always buffer-file-name)
           (:ignore partial-recall--mapped-buffer-p)
           cancel-timer
           run-at-time)

      (setq partial-recall--timer 'timer)
      (partial-recall--on-buffer-list-update)

      (bydi-was-called cancel-timer)

      (bydi-was-called run-at-time))))

(ert-deftest pr--on-frame-delete ()
  (defvar tab-bar-tabs-function nil)

  (let ((tab-bar-tabs-function (lambda (_) '(one two))))
    (bydi (partial-recall--on-close
           tab-bar-tabs)

      (partial-recall--on-frame-delete 'frame)

      (bydi-was-called-n-times partial-recall--on-close 2))))

;;; handlers-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

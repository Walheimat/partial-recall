;;; conditionals-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(ert-deftest pr--memory-buffer-p ()
  (with-tab-history :pre t

    (let ((memory (gethash (alist-get 'pr test-tab) partial-recall--table)))

      (should (partial-recall--memory-buffer-p (current-buffer) memory)))))

(ert-deftest pr-moment-buffer-p ()
  (with-tab-history :pre t

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory))
           (moment (ring-ref ring 0)))

      (should (partial-recall--moment-buffer-p (current-buffer) moment)))))

(ert-deftest pr--mapped-buffer-p ()
  (with-tab-history :pre t

    (should (partial-recall--mapped-buffer-p (current-buffer)))))

(ert-deftest pr--reality-buffer-p ()
  (with-tab-history :pre t
    (should (partial-recall--reality-buffer-p (current-buffer)))))

(ert-deftest pr--reality-owns-buffer-p ()
  (with-tab-history :pre t
    (should (partial-recall--reality-owns-buffer-p (current-buffer)))))

(ert-deftest pr--should-extend-memory-p ()
  (let ((seconds '(10 11 12))
        (partial-recall-buffer-limit 1)
        (partial-recall-max-age 2))

    (with-tab-history
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

        (partial-recall--remember (current-buffer))

        (let ((memory (partial-recall--reality)))

          (should (partial-recall--should-extend-memory-p memory))
          (should-not (partial-recall--should-extend-memory-p memory)))))))

(ert-deftest pr--has-buffers-p ()
  (with-tab-history
    (should-not (partial-recall--has-buffers-p))

    (partial-recall--remember (current-buffer))

    (should (partial-recall--has-buffers-p))))

;;; conditionals-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

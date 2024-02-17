;;; partial-recall-plasticity-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-plasticity'.

;;; Code:

(require 'partial-recall-plasticity)

(ert-deftest prp--should-extend-memory-p ()
  :tags '(plasticity needs-history)

  (let ((seconds '(10 11 12))
        (partial-recall-memory-size 1)
        (partial-recall-intermediate-term 2))

    (with-tab-history nil
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

        (partial-recall--remember (current-buffer))

        (let ((memory (partial-recall--reality)))

          (should (partial-recall-plasticity--should-extend-p memory))
          (should-not (partial-recall-plasticity--should-extend-p memory)))))))

;;; partial-recall-plasticity-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

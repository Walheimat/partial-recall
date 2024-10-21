;;; partial-recall-plasticity-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-plasticity'.

;;; Code:

(require 'partial-recall-plasticity)

(ert-deftest prp--should-extend-memory-p--if-oldest-is-young ()
  :tags '(plasticity needs-history)

  (let ((seconds '(10 11 12 13))
        (partial-recall-memory-size 1)
        (partial-recall-intermediate-term 2))

    (with-tab-history nil
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

        (partial-recall--remember (current-buffer))

        (let ((memory (partial-recall--reality)))

          (should (partial-recall-plasticity--should-extend-p memory))
          (should-not (partial-recall-plasticity--should-extend-p memory)))))))

(ert-deftest prp--maybe-implant-moment ()
  :tags '(needs-history plasticity)

  (with-moment-plasticity-enabled
    (let ((partial-recall-plasticity-implant-threshold 2)
          (partial-recall-intensities '((reinsert . 1))))
      (with-tab-history (:pre t)

        (bydi ((:spy partial-recall-plasticity--maybe-implant-or-excise)
               (:spy partial-recall--set-permanence)
               partial-recall-moment--reset-count)

          (partial-recall--reinforce (current-buffer))
          (partial-recall--reinforce (current-buffer))
          (partial-recall--reinforce (current-buffer))

          (bydi-was-called-n-times partial-recall-plasticity--maybe-implant-or-excise 2)
          (bydi-was-called-n-times partial-recall--set-permanence 1)

          ;; Test moving the moment below the threshold again.
          (let ((moment (partial-recall-current-moment))
                (partial-recall-intensities '((reinsert . -1))))
            (should (partial-recall-moment--permanence moment))

            (partial-recall--reinforce (current-buffer))

            (should-not (partial-recall-moment--permanence moment)))

          (partial-recall--reinforce (current-buffer))

          (partial-recall--set-permanence (current-buffer) t)

          (bydi-was-called partial-recall-moment--reset-count))))))

(ert-deftest pr-remember--reinforces-permanent ()
  :tags '(needs-history)

  (with-moment-plasticity-enabled
    (let ((partial-recall-memory-size 1))

      (with-tab-history (:pre t :probes t)
        (let ((another-temp (generate-new-buffer " *temp*" t)))

          (partial-recall--set-permanence)

          (bydi ((:spy partial-recall--reinsert))
            (partial-recall--remember another-temp)

            (bydi-was-called partial-recall--reinsert))

          (kill-buffer another-temp))))))

;;; partial-recall-plasticity-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

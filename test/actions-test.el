;;; actions-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(ert-deftest pr-remember--remembers ()
  (with-tab-history

    (partial-recall--remember (current-buffer))

    (should-not (null (gethash (alist-get 'pr test-tab) partial-recall--table)))))

(ert-deftest pr-remember--inserts-once ()
  (defvar partial-recall--table)

  (with-tab-history :pre t

    (partial-recall--remember (current-buffer))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 1 (ring-length ring))))))

(ert-deftest pr-remember--extends-ring ()
  (with-tab-history
    (bydi ((:always partial-recall--memory-at-capacity-p)
           (:always partial-recall--should-extend-memory-p)
           ring-extend)

      (partial-recall--remember (current-buffer))
      (bydi-was-called ring-extend))))

(ert-deftest pr-remember--reinforces-permanent ()
  (let ((partial-recall-buffer-limit 1))
    (with-tab-history :pre t
      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--implant)

        (partial-recall--remember another-temp)

        (should (eq 2 (partial-recall--update-count)))

        (kill-buffer another-temp)))))

(ert-deftest pr-remember--removes-impermanent ()
  (let ((partial-recall-buffer-limit 1))
    (with-tab-history :pre t
      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (bydi ((:always partial-recall--memory-at-capacity-p)
               (:ignore partial-recall--should-extend-memory-p))
          (partial-recall--remember another-temp))

        (should (eq 1 (ring-size (partial-recall--memory-ring (partial-recall--reality)))))

        (kill-buffer another-temp)))))

(ert-deftest pr-swap ()
  (let* ((partial-recall-buffer-limit 1)
         (a (partial-recall--memory-create "a"))
         (b (partial-recall--memory-create "b"))
         (buffer (generate-new-buffer " *temp*" t))
         (moment (partial-recall--moment-create buffer)))

    (ring-insert (partial-recall--memory-ring a) moment)

    (bydi ((:mock partial-recall--tab-name :return "tab")
           (:mock buffer-name :return "buffer")
           partial-recall--maybe-reinforce-implanted
           partial-recall--moment-update-timestamp
           (:always partial-recall--memory-at-capacity-p)
           (:always partial-recall--should-extend-memory-p))

      (partial-recall--swap a b moment)

      (bydi-was-called partial-recall--moment-update-timestamp)

      (let ((ring (partial-recall--memory-ring b)))

        (should (eq 2 (ring-size ring)))
        (should (ring-member ring moment))))))

(ert-deftest pr-recollect--reinforces-reality-or-reclaims ()
  (bydi ((:sometimes partial-recall--reality-buffer-p)
         partial-recall--reinforce
         partial-recall--reclaim)
    (partial-recall--recollect (current-buffer))

    (bydi-was-called partial-recall--reinforce)
    (bydi-toggle-sometimes)

    (let ((partial-recall-reclaim t))

      (partial-recall--recollect (current-buffer))
      (bydi-was-called partial-recall--reclaim)

      (bydi-clear-mocks)

      (setq partial-recall-reclaim nil)
      (partial-recall--recollect (current-buffer))
      (bydi-was-not-called partial-recall--reclaim))))

(ert-deftest pr-reinforce--reinforces-old-buffers ()
  (with-tab-history :pre t
    (should-not (partial-recall--reinforce (current-buffer)))
    (should (partial-recall--reinforce (current-buffer) t)))

  (with-tab-history
    (let ((count nil)
          (get-count (lambda ()
                       (let* ((reality (partial-recall--reality))
                              (moments (partial-recall--memory-ring reality))
                              (buffer (current-buffer))
                              (moment (ring-ref moments (partial-recall--moments-member moments buffer))))
                         (partial-recall--moment-update-count moment))))
          (partial-recall-buffer-limit 2)
          (another-temp (generate-new-buffer " *temp*" t)))

      (partial-recall--remember (current-buffer))

      (setq count (funcall get-count))

      (partial-recall--remember another-temp)
      (should (partial-recall--reinforce (current-buffer)))
      (should-not (eq count (funcall get-count)))

      (kill-buffer another-temp))))

(ert-deftest pr-reclaim--reclaims-from-other ()
  (let ((seconds '(10 12))
        (partial-recall-reclaim-min-age 0)
        (mock-reality (partial-recall--memory-create "other-key")))

    (with-tab-history
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))
        (partial-recall--remember (current-buffer))

        (bydi ((:mock partial-recall--reality :return mock-reality)
               partial-recall--swap)

          (partial-recall--reclaim (current-buffer))

          (bydi-was-called partial-recall--swap))))))

(ert-deftest pr-reclaim--no-op-for-same ()
  (with-tab-history :pre t

    (bydi (partial-recall--remember)

      (partial-recall--reclaim (current-buffer))

      (bydi-was-not-called partial-recall--remember))))

(ert-deftest pr-forget--forgets ()
  (with-tab-history :pre t

    (bydi (partial-recall--suppress)
      (partial-recall--forget (current-buffer) t)

      (bydi-was-called partial-recall--suppress))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 0 (ring-length ring))))))

(ert-deftest pr-forget--clears-subconscious ()
  (with-tab-history :pre t :lifts t

    (let ((sub (partial-recall--ensure-subconscious)))

      (partial-recall--forget (current-buffer) t)

      (should (partial-recall--memory-buffer-p (current-buffer) sub))

      (partial-recall--forget (current-buffer))

      (should-not (partial-recall--memory-buffer-p (current-buffer) sub)))))

(ert-deftest pr-forget--shortens-extended-memory ()
  (let ((partial-recall-buffer-limit 2)
        (another-temp (generate-new-buffer " *temp*" t))
        (yet-another-temp (generate-new-buffer " *temp*" t)))

    (bydi (partial-recall--suppress)
      (with-tab-history :pre t
        (partial-recall--remember another-temp)
        (partial-recall--remember yet-another-temp)

        (should (eq (ring-size (partial-recall--memory-ring (partial-recall--reality)))
                    3))

        (partial-recall--forget another-temp)

        (should (eq (ring-size (partial-recall--memory-ring (partial-recall--reality)))
                    2))

        (partial-recall--forget yet-another-temp)

        (should (eq (ring-size (partial-recall--memory-ring (partial-recall--reality)))
                    2))

        (kill-buffer another-temp)
        (kill-buffer yet-another-temp)))))

(ert-deftest partial-recall--suppress--remembers ()
  (with-tab-history :pre t
    (partial-recall--forget (current-buffer) t)

    (should (eq (ring-length (partial-recall--memory-ring (gethash partial-recall--subconscious-key partial-recall--table)))
                1))))

(ert-deftest partial-recall--suppress--kills ()
  (with-tab-history

    (let ((another-temp (generate-new-buffer " *temp*" t)))

      (partial-recall--remember another-temp)
      (partial-recall--remember (current-buffer))

      (partial-recall--forget another-temp t)

      (bydi ((:always partial-recall--memory-at-capacity-p)
             kill-buffer)
        (partial-recall--forget (current-buffer) t)

        (bydi-was-called-with kill-buffer another-temp))

      (kill-buffer another-temp))))

(ert-deftest partial-recall--suppress--just-removes ()
  (let ((partial-recall-repress nil))

    (with-tab-history

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (partial-recall--remember (current-buffer))
        (partial-recall--forget another-temp)

        (bydi ((:always partial-recall--memory-at-capacity-p)
               kill-buffer)
          (partial-recall--forget)

          (bydi-was-not-called kill-buffer))

        (kill-buffer another-temp)))))

(ert-deftest partial-recall--lift ()
  (with-tab-history :lifts t :pre t
    (let ((sub (gethash partial-recall--subconscious-key partial-recall--table)))

      (partial-recall--forget (current-buffer) t)

      (should (eq (ring-length (partial-recall--memory-ring sub)) 1))

      (partial-recall--lift (current-buffer))

      (should (eq (ring-length (partial-recall--memory-ring sub)) 0)))))

(ert-deftest pr--maybe-implant-moment ()
  (let ((partial-recall-auto-implant t)
        (partial-recall-auto-implant-threshold 1))
    (with-tab-history :pre t
      (partial-recall--reinforce (current-buffer) t)
      (partial-recall--reinforce (current-buffer) t)

      (let ((moment (partial-recall--moment-from-buffer (current-buffer))))
        (should (partial-recall--moment-permanence moment))))))

;;; actions-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

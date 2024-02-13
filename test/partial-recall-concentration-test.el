;;; partial-recall-concentration-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-concentration'.

;;; Code:

(require 'partial-recall-concentration)

(ert-deftest pr--concentrate--defers-for-not-owned ()
  :tags '(concentration)

  (bydi ((:ignore partial-recall--find-owning-moment)
         (:ignore partial-recall--buffer-in-memory-p)
         (:always partial-recall--reality)
         partial-recall-concentration--defer)

    (partial-recall-concentration--concentrate)

    (bydi-was-called partial-recall-concentration--defer)))

(ert-deftest pr--concentrate--defers-for-not-ready ()
  :tags '(concentration)

  (bydi ((:ignore partial-recall--reality)
         partial-recall-concentration--defer)

    (partial-recall-concentration--concentrate)

    (bydi-was-called partial-recall-concentration--defer)))

(ert-deftest pr--concentrate--renews-after-change ()
  :tags '(concentration)

  (with-tab-history (:pre t)
    (let ((partial-recall--last-focus (partial-recall-current-moment)))

      (bydi ((:mock partial-recall--find-owning-moment :return 'new)
             partial-recall-concentration--renew
             (:watch partial-recall--last-focus)
             (:ignore partial-recall--buffer-visible-p))

        (partial-recall-concentration--concentrate)

        (bydi-was-called partial-recall-concentration--renew)
        (bydi-was-set-to partial-recall--last-focus 'new)))))

(ert-deftest pr--hold-concentration--can-hold ()
  :tags '(concentration)

  (with-tab-history (:pre t)

    (let* ((partial-recall--last-focus (partial-recall-current-moment))
           (partial-recall--faint-focus nil)
           (another (generate-new-buffer " *temp*" t))
           (another-moment (partial-recall-moment--create another)))

      (bydi (partial-recall-moment--intensify
             partial-recall-concentration--renew
             (:mock partial-recall--find-owning-moment :return another)
             (:always partial-recall--buffer-visible-p)
             (:watch partial-recall--faint-focus))

        (partial-recall-concentration--concentrate)

        (bydi-was-called-with partial-recall-moment--intensify `(,partial-recall--last-focus ...))

        (kill-buffer another)

        (setq partial-recall--last-focus nil
              partial-recall--faint-focus (partial-recall-current-moment))

        (bydi-clear-mocks-for 'partial-recall--faint-focus)

        (partial-recall-concentration--concentrate)

        (bydi-was-called partial-recall-concentration--renew)
        (bydi-was-called partial-recall-moment--intensify)
        (bydi-was-set partial-recall--faint-focus)))))

(ert-deftest pr--shift-or-defer-concentration ()
  :tags '(concentration)

  (let ((partial-recall-concentration--deferred nil)
        (partial-recall--last-focus 'last)
        (partial-recall-handle-delay 5)
        (partial-recall-concentration-cycle 10))

    (bydi (partial-recall-concentration--start
           (:watch partial-recall-concentration--deferred)
           (:watch partial-recall--faint-focus))

      (partial-recall-concentration--shift "test")

      (bydi-was-called partial-recall-concentration--start)
      (bydi-was-not-set partial-recall-concentration--deferred)

      (partial-recall-concentration--defer)

      (bydi-was-set partial-recall-concentration--deferred)
      (bydi-was-set-to partial-recall--faint-focus 'last)

      (bydi-was-called-with partial-recall-concentration--start '(nil 1) t)

      (partial-recall-concentration--defer)

      (bydi-was-set-to partial-recall--faint-focus nil)
      (bydi-was-not-called partial-recall-concentration--start))))

(ert-deftest pr--renew-concentration ()
  :tags '(concentration)

  (let ((partial-recall-concentration--deferred nil)
        (partial-recall-concentration-cycle 10))

    (bydi (partial-recall-concentration--start
           (:watch partial-recall-concentration--deferred))

      (partial-recall-concentration--renew)

      (bydi-was-not-called partial-recall-concentration--start)
      (bydi-was-not-set partial-recall-concentration--deferred)

      (setq partial-recall-concentration--deferred t)

      (bydi-clear-mocks-for 'partial-recall-concentration--deferred)

      (partial-recall-concentration--renew)

      (bydi-was-called-with partial-recall-concentration--start 10)
      (bydi-was-set partial-recall-concentration--deferred))))

(ert-deftest pr--start-concentration ()
  :tags '(concentration)

  (let ((partial-recall-concentration--timer nil)
        (partial-recall-handle-delay 1)
        (partial-recall-concentration-cycle 10))

    (bydi (cancel-timer
           run-with-timer)

      (partial-recall-concentration--shift "test")

      (bydi-was-not-called cancel-timer)

      (bydi-was-called-with run-with-timer '(2 11 partial-recall-concentration--concentrate))

      (setq partial-recall-concentration--timer 'timer)

      (partial-recall-concentration--start 5 5)

      (bydi-was-called cancel-timer)
      (bydi-was-called-with run-with-timer '(6 6 partial-recall-concentration--concentrate)))))

(ert-deftest prcon--setup-and-teardown ()
  :tags '(concentration)

  (bydi (cancel-timer
         partial-recall-concentration--concentrate
         (:always partial-recall--reality)
         (:risky-mock add-hook :with bydi-rf)
         (:risky-mock advice-add :with bydi-rf)
         (:risky-mock remove-hook :with bydi-rf)
         (:risky-mock advice-remove :with bydi-rf))

    (partial-recall-concentration--setup)

    (bydi-was-called partial-recall-concentration--concentrate)
    (bydi-was-called add-hook)
    (bydi-was-called advice-add)

    (partial-recall-concentration--teardown)

    (bydi-was-called cancel-timer)
    (bydi-was-called remove-hook)
    (bydi-was-called advice-remove)))

(ert-deftest prcon--mode ()
  :tags '(concentration)

  (bydi (partial-recall-concentration--setup
         partial-recall-concentration--teardown)

    (partial-recall-concentration-mode)

    (bydi-was-called partial-recall-concentration--setup)

    (partial-recall-concentration-mode -1)

    (bydi-was-called partial-recall-concentration--teardown)))

;;; partial-recall-concentration-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

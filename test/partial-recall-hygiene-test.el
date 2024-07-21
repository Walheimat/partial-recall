;;; partial-recall-hygiene-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-hygiene'.

;;; Code:

(require 'partial-recall-hygiene nil t)

(ert-deftest pr-hygiene--nag ()
  :tags '(hygiene)

  (let ((partial-recall-hygiene-nag-buffer-action 'test)
        (partial-recall-hygiene-nag-buffer-action-params '((mock . turtle))))

    (bydi (display-buffer
           select-window)

      (partial-recall-hygiene--nag "Testing")

      (should (get-buffer partial-recall-hygiene--nag-buffer-name))

      (bydi-was-called-with display-buffer '(... (test . ((mock . turtle)))))
      (bydi-was-called select-window))))

(ert-deftest pr-hygiene--on-idle--flushes ()
  :tags '(hygiene)

  (let ((partial-recall-hygiene-warn-on-full nil))

    (bydi (partial-recall-log
           (:mock partial-recall-memories :return '(a b c))
           (:mock partial-recall--flush :return 1))

      (partial-recall-hygiene--on-idle)

      (bydi-was-called-n-times partial-recall--flush 3)
      (bydi-was-called-n-times partial-recall-log 2))))

(ert-deftest pr-hygiene--on-idle--nags ()
  :tags '(hygiene)

  (let ((partial-recall-hygiene-flush nil))

    (bydi (partial-recall-log
           partial-recall-hygiene--nag
           (:mock partial-recall-memories :return '(a b c))
           (:always partial-recall-memory--near-capacity-p)
           (:mock partial-recall-memory--name :with symbol-name)
           (:mock partial-recall-memory--unique :with symbol-name))

      (partial-recall-hygiene--on-idle)

      (bydi-was-called partial-recall-hygiene--nag t)

      (partial-recall-hygiene--on-idle)

      (bydi-was-not-called partial-recall-hygiene--nag))))

(ert-deftest pr-hygiene-mode ()
  :tags '(hygiene)

  (bydi ((:mock run-with-idle-timer :return 'timer)
         (:watch partial-recall-hygiene--timer)
         cancel-timer)

    (partial-recall-hygiene-mode +1)

    (bydi-was-called run-with-idle-timer)
    (bydi-was-set-to partial-recall-hygiene--timer 'timer)

    (partial-recall-hygiene-mode -1)

    (bydi-was-called cancel-timer)
    (bydi-was-set-to-last partial-recall-hygiene--timer nil)))

;;; partial-recall-hygiene-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; partial-recall-hygiene-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-hygiene'.

;;; Code:

(require 'partial-recall-hygiene nil t)

(ert-deftest pr-hygiene--on-idle ()
  :tags '(hygiene)

  (bydi (partial-recall-log
         (:mock partial-recall-memories :return '(a b c))
         (:mock partial-recall--flush :return 1))

    (partial-recall-hygiene--on-idle)

    (bydi-was-called-n-times partial-recall--flush 3)
    (bydi-was-called-n-times partial-recall-log 2)))

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

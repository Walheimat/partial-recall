;;; partial-recall-hygiene-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-hygiene'.

;;; Code:

(require 'partial-recall-hygiene nil t)

(ert-deftest pr-hygiene--message ()
  :tags '(hygiene)

  (bydi ((:watch partial-recall-log)
         partial-recall-log)

    (partial-recall-hygiene--message "Testing")

    (bydi-was-set partial-recall-log)

    (bydi-was-called-with partial-recall-log "Testing")))

(ert-deftest pr-hygiene--on-idle--flushes ()
  :tags '(hygiene)

  (let ((partial-recall-hygiene-warn-on-full nil))

    (bydi (partial-recall-debug
           (:mock partial-recall-memories :return '(a b c))
           (:mock partial-recall--flush :return 1))

      (partial-recall-hygiene--on-idle)

      (bydi-was-called-n-times partial-recall--flush 3)
      (bydi-was-called-n-times partial-recall-debug 2))))

(ert-deftest pr-hygiene--on-idle--nags ()
  :tags '(hygiene)

  (let ((partial-recall-hygiene-flush nil))

    (bydi (partial-recall-log
           partial-recall-hygiene--message
           (:mock partial-recall-memories :return '(a b c))
           (:always partial-recall-memory--near-capacity-p)
           (:mock partial-recall-memory--name :with symbol-name)
           (:mock partial-recall-memory--unique :with symbol-name))

      (partial-recall-hygiene--on-idle)

      (bydi-was-called partial-recall-hygiene--message :clear t)

      (partial-recall-hygiene--on-idle)

      (bydi-was-not-called partial-recall-hygiene--message))))

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

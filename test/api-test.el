;;; api-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(ert-deftest pr-mode ()
  (bydi (partial-recall-mode--setup
         partial-recall-mode--teardown)

    (partial-recall-mode 1)

    (bydi-was-called partial-recall-mode--setup)

    (partial-recall-mode -1)

    (bydi-was-called partial-recall-mode--teardown)))

(ert-deftest pr--api ()
  (let ((partial-recall-mode nil))

    (bydi (partial-recall--reinforce
           partial-recall--reclaim
           partial-recall--forget
           partial-recall--complete-dream
           partial-recall--complete-reality
           partial-recall--complete-subconscious
           partial-recall--implant
           partial-recall--lift
           switch-to-buffer)

      (call-interactively 'partial-recall-reinforce)
      (call-interactively 'partial-recall-reclaim)
      (call-interactively 'partial-recall-forget)
      (call-interactively 'partial-recall-switch-to-buffer)
      (call-interactively 'partial-recall-implant)
      (call-interactively 'partial-recall-lift)

      (bydi-was-called partial-recall--reinforce)
      (bydi-was-called partial-recall--reclaim)
      (bydi-was-called partial-recall--forget)
      (bydi-was-called partial-recall--complete-dream)
      (bydi-was-called partial-recall--complete-reality)
      (bydi-was-called partial-recall--implant)
      (bydi-was-called partial-recall--lift)
      (bydi-was-called switch-to-buffer))))

;;; api-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

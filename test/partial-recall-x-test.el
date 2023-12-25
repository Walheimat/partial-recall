;;; partial-recall-x-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-x'.

;;; Code:

(require 'partial-recall-x nil t)

(ert-deftest buffer-specs ()
  :tags '(needs-history)

  (with-tab-history (:pre t)
    (partial-recall--implant)

    (should (equal '(:meaningful t :real t :implanted t)
                   (partial-recall-x-buffer-specs)))

    (bydi-with-mock (partial-recall-log)
      (call-interactively 'partial-recall-x-buffer-specs)

      (bydi-was-called partial-recall-log))))

(ert-deftest memory-specs ()
  :tags '(needs-history)

  (with-tab-history (:pre t)
    (should (equal '(:size 1 :capacity 10 :original-capacity 10)
                   (partial-recall-x-memory-specs)))

    (bydi-with-mock (partial-recall-log)
      (call-interactively 'partial-recall-x-memory-specs)

      (bydi-was-called partial-recall-log))))

;;; partial-recall-x-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; partial-recall-extensions-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-extensions'.

;;; Code:

(require 'partial-recall-extensions nil t)

(ert-deftest pr-buffer-specs ()
  :tags '(history)

  (with-tab-history :pre t
    (partial-recall--implant)

    (should (equal '(:meaningful t :real t :implanted t)
                   (partial-recall-buffer-specs)))

    (bydi-with-mock (partial-recall--message)
      (call-interactively 'partial-recall-buffer-specs)

      (bydi-was-called partial-recall--message))))

(ert-deftest pr-memory-specs ()
  :tags '(history)

  (with-tab-history :pre t :probes t

    (should (equal '(:size 1 :capacity 10 :original-capacity 10)
                   (partial-recall-memory-specs)))

    (bydi-with-mock (partial-recall--message)
      (call-interactively 'partial-recall-memory-specs)

      (bydi-was-called partial-recall--message))))

;;; partial-recall-extensions-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; utility-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(ert-deftest pr--warn ()
  (bydi (display-warning)
    (partial-recall--warn "Testing")
    (bydi-was-called-with display-warning (list 'partial-recall "Testing" :warning))))

(ert-deftest pr--log ()
  (let ((partial-recall--log nil))

    (should-not (partial-recall--log "test: %s %s" "one" "two"))

    (partial-recall-toggle-logging)

    (ert-with-message-capture messages
      (partial-recall--log "test: %s %s" "one" "two")

      (should (string= messages "test: one two\n")))))

;;; utility-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

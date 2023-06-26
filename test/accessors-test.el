;;; accessors-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(ert-deftest pr--key--returns-if-set ()
  (bydi ((:mock tab-bar--current-tab :return test-tab))
    (should (string= "test-hash" (partial-recall--key)))))

(ert-deftest pr--create-key ()
  (bydi ((:mock random :return 42)
         (:mock emacs-pid :return 1)
         (:mock recent-keys :return 'keys)
         md5)

    (partial-recall--create-key "test")
    (bydi-was-called-with md5 "test421keys")))

(ert-deftest pr-recall--buffer-owner ()
  (with-tab-history :pre t

    (let ((memory (partial-recall--reality)))

      (should (eq memory (partial-recall--buffer-owner))))))

(ert-deftest pr--update-count ()
  (let ((partial-recall-buffer-limit 2))

    (with-tab-history :pre t

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (partial-recall--reinforce (current-buffer))

        (should (eq 1 (partial-recall--update-count)))
        (kill-buffer another-temp)))))

(ert-deftest pr--tab ()
  (with-tab-history :pre t
    (should (string= (partial-recall--tab-name) "test-tab"))))

(ert-deftest pr--ensure-subconscious ()
  (let ((partial-recall-table (make-hash-table)))

    (partial-recall--ensure-subconscious)

    (should (gethash partial-recall--subconscious-key partial-recall--table))))

(ert-deftest partial-recall--lifted ()
  (with-tab-history :pre t :lifts t
    (partial-recall--forget (current-buffer) t)

    (should (partial-recall--lifted (current-buffer)))))

;;; accessors-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

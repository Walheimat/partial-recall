;;; completion-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

;; Completion

(ert-deftest pr--complete-dream ()
  (bydi-with-mock ((:mock completing-read :return "second")
                   (:ignore partial-recall--reality-owns-buffer-p)
                   (:mock partial-recall--mapped-buffers :return '("first" "second" "third"))
                   (:mock buffer-name :with bydi-rf))
    (should (string= "second" (partial-recall--complete-dream "Some prompt: ")))))

(ert-deftest pr--complete-reality ()
  (bydi-with-mock ((:mock completing-read :return "second")
                   (:always partial-recall--reality-owns-buffer-p)
                   (:mock partial-recall--mapped-buffers :return '("first" "second" "third"))
                   (:mock buffer-name :with bydi-rf))
    (should (string= "second" (partial-recall--complete-reality "Some prompt: ")))))

(ert-deftest pr--complete-subconscious ()
  (let* ((partial-recall--table (make-hash-table))
         (buf (generate-new-buffer " *temp*" t))
         (name (buffer-name buf))
         (sub (partial-recall--ensure-subconscious)))

    (ring-insert (partial-recall--memory-ring sub)
                 (partial-recall--moment-create buf))

    (bydi-with-mock ((:mock completing-read :return name))

      (should (equal buf (partial-recall--complete-subconscious "Some prompt: "))))))

;;; completion-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

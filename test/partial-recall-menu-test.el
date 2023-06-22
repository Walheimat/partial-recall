;;; partial-recall-menu-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-menu'.

(require 'partial-recall-menu nil t)

(ert-deftest prm--revert ()
  (bydi (tabulated-list-init-header
         (:mock partial-recall-menu--print-update-count :return "0")
         (:mock partial-recall-menu--print-timestamp :return "today")
         (:mock partial-recall-menu--print-memory :return "rem")
         (:mock partial-recall-menu--print-permanence :return "*"))

    (with-tab-history :pre t

      (partial-recall-menu--revert)

      (should (equal `((("test-tab" ,(current-buffer) t) [,(buffer-name) "rem" "today" "*" "0"]))
                     tabulated-list-entries))
      (should (equal partial-recall-menu--list-format tabulated-list-format))
      (bydi-was-called tabulated-list-init-header))))

(ert-deftest prm--list ()
  (bydi (partial-recall-menu-mode
         partial-recall-menu--revert
         tabulated-list-print)

    (let* ((buf (generate-new-buffer " *temp*" t))
           (partial-recall-menu--buffer (buffer-name buf)))

      (should (eq (partial-recall-menu--list) buf))
      (bydi-was-called partial-recall-menu-mode)
      (bydi-was-called partial-recall-menu--revert)
      (bydi-was-called tabulated-list-print)

      (kill-buffer buf))))

(ert-deftest prm--entry ()
  (bydi ((:sometimes tabulated-list-get-id))

    (should (partial-recall-menu--entry))

    (bydi-toggle-sometimes)

    (should-error (partial-recall-menu--entry) :type 'error)))

(ert-deftest prm--switch ()
  (let ((real nil))
    (bydi ((:mock partial-recall-menu--entry :return (list "tab" 'buffer real))
           tab-bar-switch-to-tab
           switch-to-buffer)

      (should-error (partial-recall-menu--switch #'switch-to-buffer t))

      (partial-recall-menu--switch #'switch-to-buffer)

      (bydi-was-called tab-bar-switch-to-tab)
      (bydi-was-called switch-to-buffer)

      (setq real t)
      (bydi-clear-mocks)

      (partial-recall-menu--switch #'switch-to-buffer)
      (bydi-was-not-called tab-bar-switch-tab)
      (bydi-was-called switch-to-buffer))))

;; Utility

(ert-deftest prm--print-timestamp ()
  (bydi (format-time-string
         (:mock seconds-to-time :return 'time))
    (partial-recall-menu--print-timestamp 1)

    (bydi-was-called-with seconds-to-time 1)
    (bydi-was-called-with format-time-string (list "%H:%M:%S" 'time))))

(ert-deftest prm--print-update-count ()
  (let ((partial-recall-menu--null "0"))

    (should (string= "0" (partial-recall-menu--print-update-count 0)))
    (should (string= "1" (partial-recall-menu--print-update-count 1)))))

(ert-deftest prm--print-permanence ()
   (should (string= "-" (partial-recall-menu--print-permanence nil)))
   (should (string= "*" (partial-recall-menu--print-permanence t))))

(ert-deftest prm--print-memory ()
  (let ((partial-recall-buffer-limit 10)
        (partial-recall-menu--null "0"))

    (with-tab-history :pre t
      (should (string= "0" (partial-recall-menu--print-memory (partial-recall--reality) t)))
      (should (string= "test-tab" (partial-recall-menu--print-memory (partial-recall--reality) nil)))

      (ring-resize (partial-recall--memory-ring (partial-recall--reality)) 11)

      (should (string= "test-tab (+1)" (partial-recall-menu--print-memory (partial-recall--reality) nil))))))

(ert-deftest prm-mode ()
  (with-temp-buffer
    (partial-recall-menu-mode)

    (eq '(partial-recall-menu--revert) tabulated-list-revert-hook)))

;; API

(ert-deftest prm-api ()
  (let ((real nil))

    (bydi (partial-recall-menu--switch
           (:mock partial-recall-menu--entry :return (list "tab" 'buffer real))
           partial-recall--reclaim
           partial-recall--reinforce
           partial-recall--forget
           partial-recall--implant
           display-buffer
           (:mock partial-recall-menu--list :return 'list))

      (partial-recall-menu-switch-to-buffer)
      (bydi-was-called-with partial-recall-menu--switch (list #'switch-to-buffer))

      (partial-recall-menu-switch-to-buffer-other-window)
      (bydi-was-called-with partial-recall-menu--switch (list #'switch-to-buffer-other-window t))

      (partial-recall-menu-reclaim-buffer)
      (bydi-was-called-with partial-recall--reclaim (list 'buffer t))
      (setq real t)
      (should-error (partial-recall-menu-reclaim-buffer))

      (partial-recall-menu-reinforce-buffer)
      (bydi-was-called-with partial-recall--reinforce (list 'buffer t))
      (setq real nil)
      (should-error (partial-recall-menu-reinforce-buffer))

      (partial-recall-menu-forget-buffer)
      (bydi-was-called-with partial-recall--forget (list 'buffer))

      (partial-recall-menu-implant-buffer)
      (bydi-was-called-with partial-recall--implant (list 'buffer nil))

      (partial-recall-menu)
      (bydi-was-called-with display-buffer (list 'list)))))

;;; Code:

;;; partial-recall-menu-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

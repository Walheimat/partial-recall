;;; menu-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-menu'.

(require 'partial-recall-menu nil t)

(ert-deftest prm--revert ()
  (bydi (tabulated-list-init-header
         (:mock partial-recall-menu--print-update-count :return "1")
         (:mock partial-recall-menu--print-timestamp :return "today")
         (:mock partial-recall-menu--print-memory :return "rem")
         (:mock partial-recall-menu--print-permanence :return "*"))

    (with-tab-history :pre t

      (partial-recall-menu--revert)

      (should (equal `((("test-tab" ,(current-buffer) t) [" " "*" "1" ,(buffer-name) "rem" "today"]))
                     tabulated-list-entries))
      (should (equal (vector
                      '("A" 1 t :pad-right 0)
                      '("I" 1 t :pad-right 0)
                      '("U" 1 t :pad-right 1)
                      `("Buffer" ,(length (buffer-name)) t)
                      '("Tab" 8 t)
                      '("Timestamp" 9 t))
                     tabulated-list-format))
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
  (let ((partial-recall-menu--empty "e"))

    (should (string= "e" (partial-recall-menu--print-update-count 0)))
    (should (string= "1" (partial-recall-menu--print-update-count 1)))
    (should (string= "+" (partial-recall-menu--print-update-count 11)))))

(ert-deftest prm--print-permanence ()
  (should (string= " " (partial-recall-menu--print-permanence nil)))
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

(ert-deftest prm-execute ()
  (let ((entries '([" "] ["C"] ["F"] nil ["I"] ["R"] ["X"])))

    (bydi ((:mock tabulated-list-get-id :return '(t buffer))
           (:mock tabulated-list-get-entry :with (lambda () (pop entries)))
           (:mock eobp :with (lambda () (null entries)))
           tabulated-list-set-col
           tabulated-list-revert
           partial-recall--reclaim
           partial-recall--forget
           partial-recall--reinforce
           partial-recall--implant
           forward-line)

      (with-temp-buffer
        (partial-recall-menu-execute)

        (bydi-was-called-n-times partial-recall--reclaim 1)
        (bydi-was-called-n-times partial-recall--forget 1)
        (bydi-was-called-n-times partial-recall--reinforce 1)
        (bydi-was-called-n-times partial-recall--implant 2)
        (bydi-was-called-n-times forward-line 7)
        (bydi-was-called-n-times tabulated-list-revert 1)))))

(ert-deftest prm-api ()
  (let ((real nil))

    (bydi (partial-recall-menu--switch
           (:mock partial-recall-menu--entry :return (list "tab" 'buffer real))
           tabulated-list-set-col
           forward-line
           display-buffer
           (:mock partial-recall-menu--list :return 'list))

      (partial-recall-menu-switch-to-buffer)
      (bydi-was-called-with partial-recall-menu--switch (list #'switch-to-buffer))

      (partial-recall-menu-switch-to-buffer-other-window)
      (bydi-was-called-with partial-recall-menu--switch (list #'switch-to-buffer-other-window t))

      (partial-recall-menu-reclaim-buffer)
      (bydi-was-called-with tabulated-list-set-col (list 0 "C" t))
      (setq real t)
      (should-error (partial-recall-menu-reclaim-buffer))

      (partial-recall-menu-reinforce-buffer)
      (bydi-was-called-with tabulated-list-set-col (list 0 "R" t))
      (setq real nil)
      (should-error (partial-recall-menu-reinforce-buffer))

      (partial-recall-menu-forget-buffer)
      (bydi-was-called-with tabulated-list-set-col (list 0 "F" t))

      (partial-recall-menu-implant-buffer)
      (bydi-was-called-with tabulated-list-set-col (list 0 "I" t))

      (funcall-interactively 'partial-recall-menu-implant-buffer t)
      (bydi-was-called-with tabulated-list-set-col (list 0 "X" t))

      (partial-recall-menu)
      (bydi-was-called-with display-buffer (list 'list)))))

;;; Code:

;;; menu-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

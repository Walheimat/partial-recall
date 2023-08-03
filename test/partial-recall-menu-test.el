;;; partial-recall-menu-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-menu'.

(require 'partial-recall-menu nil t)

(ert-deftest prm--format--works-if-empty ()
  (should (partial-recall-menu--format nil nil)))

(ert-deftest prm--format--sets-column-from-longest ()
  (let ((buffers '("a" "bb" "ccc"))
        (tabs '("dddd" "eee" "ff")))

    (should (equal (aref (partial-recall-menu--format buffers tabs) 2)
                   '("Buffer" 3 t)))
    (should (equal (aref (partial-recall-menu--format buffers tabs) 3)
                   '("Tab" 4 t)))))

(ert-deftest prm--revert ()
  (bydi (tabulated-list-init-header
         (:mock partial-recall-menu--print-timestamp :return "today")
         (:mock partial-recall-menu--print-memory :return "rem")
         (:mock partial-recall-menu--print-presence :return "*")
         (:mock partial-recall--tab-name :return "test-tab"))

    (with-tab-history :pre t :second t

      (ring-insert
       (partial-recall--memory-ring second-memory)
       (partial-recall--moment-from-buffer (current-buffer)))

      (partial-recall-menu--revert)

      (should (equal `(("test-tab" ,(current-buffer) t nil) [" " "*" ,(buffer-name) "rem" "today"])
                     (nth 1 tabulated-list-entries)))
      (should (equal (vector
                      '("A" 1 t :pad-right 0)
                      '("P" 1 t :pad-right 1)
                      `("Buffer" ,(length (buffer-name)) t)
                      `("Tab" ,(length "test-tab") t)
                      '("Timestamp" 9 t))
                     tabulated-list-format))
      (bydi-was-called tabulated-list-init-header)

      (partial-recall-menu--revert t)

      (should partial-recall-menu--subconscious))))

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

(ert-deftest prm--id ()
  (bydi ((:sometimes tabulated-list-get-id))

    (should (partial-recall-menu--id))

    (bydi-toggle-sometimes)

    (should-error (partial-recall-menu--id) :type 'error)))

(ert-deftest prm--switch ()
  (let ((real nil))
    (bydi ((:mock partial-recall-menu--id :return (list "tab" 'buffer real 'sub))
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
  (let ((partial-recall-menu--excess-time 2)
        (seconds '(1 6)))

    (bydi (format-time-string
           (:mock seconds-to-time :return 'time)
           (:mock time-to-seconds :with (lambda () (pop seconds))))

      (partial-recall-menu--print-timestamp 1)

      (bydi-was-called-with seconds-to-time 1)
      (bydi-was-called-with format-time-string (list "%H:%M:%S" 'time))


      (partial-recall-menu--print-timestamp 3)

      (bydi-was-called-with format-time-string (list "   %d/%m" 'time)))))

(ert-deftest prm--print-update-count ()
  (let ((partial-recall-menu--empty "e")
        (partial-recall-menu--persistence-blocks ["a" "b" "c" "d"])
        (partial-recall-menu--persistence-ratios '(0.25 0.5 0.75 1))
        (partial-recall-auto-implant-threshold 4))

    (should (string= "e" (partial-recall-menu--print-presence 0 nil)))
    (should (string= "a" (partial-recall-menu--print-presence 1 nil)))
    (should (string= "b" (partial-recall-menu--print-presence 2 nil)))
    (should (string= "c" (partial-recall-menu--print-presence 3 nil)))
    (should (string= "d" (partial-recall-menu--print-presence 4 nil)))
    (should (string= "d" (partial-recall-menu--print-presence 5 nil)))
    (should (string= "d" (partial-recall-menu--print-presence 6 t)))))

(ert-deftest prm--print-memory ()
  (let ((partial-recall-buffer-limit 10)
        (partial-recall-menu--null "0")
        (partial-recall-menu--present "1"))

    (with-tab-history :pre t
      (should (string= "1" (partial-recall-menu--print-memory (partial-recall--reality))))
      (should (string= "0" (partial-recall-menu--print-memory (partial-recall--subconscious))))
      (bydi ((:ignore partial-recall--reality-p))
        (should (string= "test-tab" (partial-recall-menu--print-memory (partial-recall--reality))))

        (ring-resize (partial-recall--memory-ring (partial-recall--reality)) 11)

        (should (string= "test-tab (+1)" (partial-recall-menu--print-memory (partial-recall--reality))))))))

(ert-deftest prm-mode ()
  (with-temp-buffer
    (partial-recall-menu-mode)

    (eq '(partial-recall-menu--revert) tabulated-list-revert-hook)))

;; API

(ert-deftest prm-execute ()
  (let ((entries '(["C"] ["C" ][" "] ["F"] nil ["I"] ["R"] ["X"]))
        (sub nil))

    (bydi ((:mock tabulated-list-get-id :with (lambda () (list t 'buffer nil sub)))
           (:mock tabulated-list-get-entry :with (lambda () (let ((res (pop entries)))
                                                         (setq sub t)
                                                         res)))
           (:mock eobp :with (lambda () (null entries)))
           tabulated-list-set-col
           tabulated-list-revert
           partial-recall--reclaim
           partial-recall--forget
           partial-recall--reinforce
           partial-recall--implant
           partial-recall--lift
           forward-line)

      (with-temp-buffer
        (partial-recall-menu-execute)

        (bydi-was-called-n-times partial-recall--reclaim 1)
        (bydi-was-called-n-times partial-recall--lift 1)
        (bydi-was-called-n-times partial-recall--forget 1)
        (bydi-was-called-n-times partial-recall--reinforce 1)
        (bydi-was-called-n-times partial-recall--implant 2)
        (bydi-was-called-n-times forward-line 8)
        (bydi-was-called-n-times tabulated-list-revert 1)))))

(ert-deftest prm-api ()
  (let ((real nil)
        (sub nil))

    (bydi (partial-recall-menu--switch
           (:mock partial-recall-menu--id :return (list "tab" 'buffer real sub))
           tabulated-list-set-col
           forward-line
           display-buffer
           (:mock partial-recall-menu--list :return 'list))

      (partial-recall-menu-switch-to-buffer)
      (bydi-was-called-with partial-recall-menu--switch #'switch-to-buffer)

      (partial-recall-menu-switch-to-buffer-other-window)
      (bydi-was-called-with partial-recall-menu--switch '(switch-to-buffer-other-window t))

      (partial-recall-menu-reclaim-buffer)
      (bydi-was-called-with tabulated-list-set-col '(... "C"))
      (setq real t)
      (should-error (partial-recall-menu-reclaim-buffer))

      (partial-recall-menu-reinforce-buffer)
      (bydi-was-called-with tabulated-list-set-col '(... "R"))
      (setq real nil)
      (should-error (partial-recall-menu-reinforce-buffer))

      (partial-recall-menu-forget-buffer)
      (bydi-was-called-with tabulated-list-set-col '(..."F"))

      (partial-recall-menu-implant-buffer)
      (bydi-was-called-with tabulated-list-set-col '(..."I"))

      (funcall-interactively 'partial-recall-menu-implant-buffer t)
      (bydi-was-called-with tabulated-list-set-col '(..."X"))

      (partial-recall-menu)
      (bydi-was-called-with display-buffer 'list))))

;;; Code:

;;; partial-recall-menu-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; partial-recall-menu-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall-menu'.

;;; Code:

(require 'partial-recall-menu nil t)

(ert-deftest prm--format--works-if-empty ()
  :tags '(menu)

  (should (partial-recall-menu--format nil nil)))

(ert-deftest prm--format--sets-column-from-longest ()
  :tags '(menu)

  (let ((buffers '("a" "bb" "ccc"))
        (tabs '("dddd" "eee" "ff")))

    (should (equal (aref (partial-recall-menu--format buffers tabs) 2)
                   '("Buffer" 3 t)))
    (should (equal (aref (partial-recall-menu--format buffers tabs) 3)
                   '("Tab" 4 t)))))

(ert-deftest prm--revert ()
  :tags '(needs-history menu user-facing)

  (bydi (tabulated-list-init-header
         (:mock partial-recall-menu--print-timestamp :return "today")
         (:mock partial-recall-menu--print-memory :return "rem")
         (:mock partial-recall-menu--print-presence :return "*")
         (:mock partial-recall-menu--tab-name :return "test-tab")
         (:mock partial-recall-menu--frame :return "frame"))

    (with-tab-history :pre t :second t

      (ring-insert
       (partial-recall--memory-ring second-memory)
       (partial-recall--moment-from-buffer (current-buffer)))

      (partial-recall-menu--revert)

      (should (equal `(("test-tab" "frame" ,(current-buffer) t nil) [" " "*" ,(buffer-name) "rem" "today"])
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
  :tags '(menu)

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
  :tags '(menu)

  (bydi ((:sometimes tabulated-list-get-id))

    (should (partial-recall-menu--id))

    (bydi-toggle-sometimes)

    (should-error (partial-recall-menu--id) :type 'error)))

(ert-deftest prm--display ()
  :tags '(menu)

  (let ((real nil))
    (bydi ((:mock partial-recall-menu--id :return (list "tab" (selected-frame) 'buffer real 'sub))
           tab-bar-switch-to-tab
           display-buffer-use-some-window)

      (partial-recall-menu--display)

      (bydi-was-called tab-bar-switch-to-tab)
      (bydi-was-called display-buffer-use-some-window)

      (setq real t)
      (bydi-clear-mocks)

      (partial-recall-menu--display)
      (bydi-was-not-called tab-bar-switch-tab)
      (bydi-was-called display-buffer-use-some-window))))

;; Utility

(ert-deftest prm--print-timestamp ()
  :tags '(menu)

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
  :tags '(menu)

  (let ((partial-recall-menu--empty "e")
        (partial-recall-menu--persistence-blocks ["a" "b" "c" "d"])
        (partial-recall-menu--persistence-indicator "f")
        (partial-recall-menu--persistence-ratios '(0.25 0.5 0.75 1))
        (partial-recall-auto-implant-threshold 4))

    (should (string= "f" (partial-recall-menu--print-presence 0 t)))
    (should (string= "e" (partial-recall-menu--print-presence 0 nil)))
    (should (string= "a" (partial-recall-menu--print-presence 1 nil)))
    (should (string= "b" (partial-recall-menu--print-presence 2 nil)))
    (should (string= "c" (partial-recall-menu--print-presence 3 nil)))
    (should (string= "d" (partial-recall-menu--print-presence 4 nil)))
    (should (string= "d" (partial-recall-menu--print-presence 5 nil)))
    (should (string= "d" (partial-recall-menu--print-presence 6 t)))))

(ert-deftest prm--print-memory ()
  :tags '(needs-history menu)

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
  :tags '(menu user-facing)

  (with-temp-buffer
    (partial-recall-menu-mode)

    (eq '(partial-recall-menu--revert) tabulated-list-revert-hook)))

;; API

(ert-deftest prm-execute ()
  :tags '(menu user-facing)

  (let ((entries '(["C"] ["C" ][" "] ["F"] nil ["I"] ["R"] ["X"]))
        (sub nil))

    (bydi ((:mock tabulated-list-get-id :with (lambda () (list t 'frame 'buffer nil sub)))
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

(ert-deftest prm-unmark ()
  :tags '(menu user-facing)

  (let ((entry ["K"]))
    (bydi ((:mock tabulated-list-get-entry :return entry)
           tabulated-list-set-col
           forward-line)

      (partial-recall-menu-unmark)

      (bydi-was-called-with tabulated-list-set-col '(0 " " t))
      (bydi-was-called forward-line)

      (bydi-clear-mocks)

      (setq entry [" "])
      (partial-recall-menu-unmark)

      (bydi-was-not-called tabulasted-list-set-col))))

(ert-deftest prm-toggle-subconscious ()
  :tags '(menu user-facing)

  (with-temp-buffer
    (should-not partial-recall-menu--subconscious)

    (bydi (tabulated-list-revert)
      (partial-recall-menu-toggle-subconscious)

      (should partial-recall-menu--subconscious)

      (bydi-was-called tabulated-list-revert))))

(ert-deftest prm-api ()
  :tags '(menu user-facing)

  (let ((real nil)
        (sub nil))

    (bydi (partial-recall-menu--display
           (:mock partial-recall-menu--id :return (list "tab" "frame" 'buffer real sub))
           tabulated-list-set-col
           forward-line
           display-buffer
           (:mock partial-recall-menu--list :return 'list))

      (partial-recall-menu-display-buffer)
      (bydi-was-called partial-recall-menu--display)

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

(ert-deftest prm--tab ()
  :tags '(menu)

  (bydi ((:mock partial-recall--tab :return "tab"))

    (should (equal (list "tab" (selected-frame) nil)
                   (partial-recall-menu--tab 'memory))))

  (bydi ((:mock
          partial-recall--tab
          :with
          (lambda (mem &optional fr) (when (string= fr "second") "tab")))
         (:mock filtered-frame-list :return '("first" "second")))

    (should (equal (list "tab" "second" t)
                   (partial-recall-menu--tab 'memory)))))

(ert-deftest prm--frame ()
  :tags '(menu)

  (bydi ((:mock partial-recall-menu--tab :return (list 'tab 'frame nil)))

    (should (eq 'frame (partial-recall-menu--frame 'memory)))))

(ert-deftest prm--tab-name ()
  :tags '(menu)

  (let ((foreign t))

    (bydi ((:mock partial-recall-menu--tab :return (list '((name . "name")) 'frame foreign)))
      (should (string= "name" (partial-recall-menu--tab-name 'memory t))))))

;;; partial-recall-menu-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

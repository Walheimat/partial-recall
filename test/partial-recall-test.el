;;; partial-recall-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))

(cl-defmacro with-tab-history (&rest body &key pre &allow-other-keys)
  "Run BODY with a clear tab history and a temp buffer.

If PRE is t, pre-remember the current buffer."
  (declare (indent defun))
  `(bydi ((:mock tab-bar--current-tab :return test-tab)
          (:mock tab-bar-tabs :return (list test-tab)))
     (let ((partial-recall--table (make-hash-table)))

       (with-temp-buffer
         ,(when pre '(partial-recall--remember (current-buffer)))
         ,@body))))

;; Hash table

(ert-deftest pr--create-key ()
  (bydi ((:mock random :return 42)
         (:mock emacs-pid :return 1)
         (:mock recent-keys :return 'keys)
         md5)

    (partial-recall--create-key "test")
    (bydi-was-called-with md5 "test421keys")))

(ert-deftest pr--key--returns-if-set ()
  (bydi ((:mock tab-bar--current-tab :return test-tab))
    (should (string= "test-hash" (partial-recall--key)))))

;; Structures

(ert-deftest pr--reality-buffer-p ()
  (with-tab-history :pre t
    (should (partial-recall--reality-buffer-p (current-buffer)))))

(ert-deftest pr--has-buffers-p ()
  (with-tab-history
    (should-not (partial-recall--has-buffers-p))

    (partial-recall--remember (current-buffer))

    (should (partial-recall--has-buffers-p))))

(ert-deftest pr--mapped-buffer-p ()
  (with-tab-history :pre t

    (should (partial-recall--mapped-buffer-p (current-buffer)))))

(ert-deftest pr--memory-buffer-p ()
  (with-tab-history :pre t

    (let ((memory (gethash (alist-get 'pr test-tab) partial-recall--table)))

      (should (partial-recall--memory-buffer-p memory (current-buffer))))))

(ert-deftest pr-moment-buffer-p ()
  (with-tab-history :pre t


    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory))
           (moment (ring-ref ring 0)))

      (should (partial-recall--moment-buffer-p moment (current-buffer))))))

;; Helpers

(ert-deftest pr--should-extend-memory-p ()
  (let ((seconds '(10 11 12))
        (partial-recall-limit 1)
        (partial-recall-max-age 2))

    (with-tab-history
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

        (partial-recall--remember (current-buffer))

        (let ((memory (partial-recall--reality)))

          (should (partial-recall--should-extend-memory-p memory))
          (should-not (partial-recall--should-extend-memory-p memory)))))))

(ert-deftest pr-recall--buffer-owner ()
  (with-tab-history :pre t

    (let ((memory (partial-recall--reality)))

      (should (eq memory (partial-recall--buffer-owner))))))

(ert-deftest pr--reality-owns-buffer-p ()
  (with-tab-history :pre t
    (should (partial-recall--reality-owns-buffer-p (current-buffer)))))

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

(ert-deftest pr--warn ()
  (bydi (display-warning)
    (partial-recall--warn "Testing")
    (bydi-was-called-with display-warning (list 'partial-recall "Testing" :warning))))

(ert-deftest pr--update-count ()
  (with-tab-history
    (setq partial-recall-buffer-limit 2)

    (partial-recall--remember (current-buffer))

    (let ((another-temp (generate-new-buffer " *temp*" t)))

      (partial-recall--remember another-temp)
      (partial-recall--reinforce (current-buffer))

      (should (eq 1 (partial-recall--update-count)))
      (kill-buffer another-temp))))

(ert-deftest pr--tab ()
  (with-tab-history :pre t
    (should (string= (partial-recall--tab-name) "test-tab"))))

;; Handlers

(ert-deftest pr--handle-buffer ()
  (with-tab-history
    (bydi (partial-recall--remember
           (:always buffer-live-p)
           (:ignore partial-recall--mapped-buffer-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--remember))))

(ert-deftest pr--on-create--sets-cdr ()
  (bydi ((:mock partial-recall--create-key :return "test"))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab))))))

(ert-deftest pr--handle-buffer--reclaims ()
  (with-tab-history
    (bydi (partial-recall--remember
           partial-recall--recollect
           (:always buffer-live-p)
           (:always partial-recall--mapped-buffer-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--recollect))))

(ert-deftest pr--on-close ()
  (with-tab-history :pre t

    (should (eq 1 (length (hash-table-keys partial-recall--table))))

    (partial-recall--on-close test-tab t)

    (should (eq 1 (length (hash-table-keys partial-recall--table))))

    (partial-recall--on-close test-tab nil)

    (should (eq 0 (length (hash-table-keys partial-recall--table))))))

(ert-deftest pr--on-buffer-list-update--cancels-running-timer ()
  (let ((partial-recall--timer nil))

    (bydi ((:always buffer-file-name)
           (:ignore partial-recall--mapped-buffer-p)
           cancel-timer
           run-at-time)

      (setq partial-recall--timer 'timer)
      (partial-recall--on-buffer-list-update)

      (bydi-was-called cancel-timer)

      (bydi-was-called run-at-time))))

(ert-deftest pr--on-frame-delete ()
  (defvar tab-bar-tabs-function nil)

  (let ((tab-bar-tabs-function (lambda (_) '(one two))))
    (bydi (partial-recall--on-close
           tab-bar-tabs)

      (partial-recall--on-frame-delete 'frame)

      (bydi-was-called-n-times partial-recall--on-close 2))))

(ert-deftest pr-remember--remembers ()
  (with-tab-history

    (partial-recall--remember (current-buffer))

    (should-not (null (gethash (alist-get 'pr test-tab) partial-recall--table)))))

(ert-deftest pr-remember--inserts-once ()
  (defvar partial-recall--table)

  (with-tab-history :pre t

    (partial-recall--remember (current-buffer))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 1 (ring-length ring))))))

(ert-deftest pr-remember--extends-ring ()
  (with-tab-history
    (bydi ((:always partial-recall--memory-at-capacity-p)
           (:always partial-recall--should-extend-memory-p)
           ring-extend)

      (partial-recall--remember (current-buffer))
      (bydi-was-called ring-extend))))

(ert-deftest pr-recollect--reinforces-reality-or-reclaims ()
  (bydi ((:sometimes partial-recall--reality-buffer-p)
         partial-recall--reinforce
         partial-recall--reclaim)
    (partial-recall--recollect (current-buffer))

    (bydi-was-called partial-recall--reinforce)
    (bydi-toggle-sometimes)
    (bydi-clear-mocks)

    (let ((partial-recall-reclaim t))

      (partial-recall--recollect (current-buffer))
      (bydi-was-called partial-recall--reclaim)

      (bydi-clear-mocks)

      (setq partial-recall-reclaim nil)
      (partial-recall--recollect (current-buffer))
      (bydi-was-not-called partial-recall--reclaim))))

(ert-deftest pr-reinforce--reinforces-old-buffers ()
  (with-tab-history :pre t
    (should-not (partial-recall--reinforce (current-buffer)))
    (should (partial-recall--reinforce (current-buffer) t)))

  (with-tab-history
    (let ((count nil)
          (get-count (lambda ()
                       (let* ((reality (partial-recall--reality))
                              (moments (partial-recall--memory-ring reality))
                              (buffer (current-buffer))
                              (moment (ring-ref moments (partial-recall--ring-member moments buffer))))
                         (partial-recall--moment-update-count moment)))))

      (setq partial-recall-buffer-limit 2)
      (partial-recall--remember (current-buffer))

      (setq count (funcall get-count))

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (should (partial-recall--reinforce (current-buffer)))
        (kill-buffer another-temp)

        (should-not (eq count (funcall get-count)))))))

(ert-deftest pr-reclaim--reclaims-from-other ()
  (let ((seconds '(10 12))
        (partial-recall-reclaim-min-age 0))
    (with-tab-history
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))
        (partial-recall--remember (current-buffer))

        (bydi ((:mock partial-recall--reality :return 'other)
               partial-recall--remember)

          (partial-recall--reclaim (current-buffer))

          (bydi-was-called partial-recall--remember))))))

(ert-deftest pr-reclaim--no-op-for-same ()
  (with-tab-history :pre t

    (bydi (partial-recall--remember)

      (partial-recall--reclaim (current-buffer))

      (bydi-was-not-called partial-recall--remember))))

(ert-deftest pr-forget--forgets ()
  (with-tab-history :pre t

    (partial-recall--forget)

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 0 (ring-length ring))))))

;; Setup

(ert-deftest pr--fix-up-primary-tab ()
  (let ((tab-bar-mode nil)
        (tab-bar-tabs-function (lambda () (list test-tab))))

    (bydi (partial-recall--on-create
           (:sometimes partial-recall--key)
           partial-recall--warn)

      (partial-recall--fix-up-primary-tab)

      (bydi-was-called partial-recall--warn)
      (bydi-was-not-called partial-recall--key)
      (bydi-was-not-called partial-recall--on-create)

      (bydi-clear-mocks)

      (setq tab-bar-mode t)

      (partial-recall--fix-up-primary-tab)
      (bydi-was-called partial-recall--key)
      (bydi-was-not-called partial-recall--on-create)

      (bydi-clear-mocks)
      (bydi-toggle-sometimes)

      (partial-recall--fix-up-primary-tab)
      (bydi-was-called partial-recall--key)
      (bydi-was-called partial-recall--on-create))))

(ert-deftest pr--setup ()
  (defvar tab-bar-tabs-function nil)
  (defvar tab-bar-mode nil)

  (let ((tab-bar-tabs-function (lambda () (list test-tab)))
        (tab-bar-mode nil))

    (bydi (add-hook
           partial-recall--queue-fix-up
           (:mock tab-bar-mode :with (lambda (_) (setq tab-bar-mode t))))

      (partial-recall-mode--setup)

      (bydi-was-called partial-recall--queue-fix-up)
      (bydi-was-called-n-times add-hook 6)
      (bydi-was-called tab-bar-mode))))

(ert-deftest pr--queue-fix-up ()
  (bydi (run-at-time)
    (partial-recall--queue-fix-up)

    (bydi-was-called-with run-at-time (list 1.0 nil #'partial-recall--fix-up-primary-tab))))

(ert-deftest pr--teardown ()
  (bydi (remove-hook)

    (partial-recall-mode--teardown)

    (bydi-was-called-n-times remove-hook 6)))

;; API

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
           switch-to-buffer)

      (partial-recall-reinforce)
      (partial-recall-reclaim)
      (partial-recall-forget)
      (call-interactively 'partial-recall-steal)
      (call-interactively 'partial-recall-switch-to-buffer)

      (bydi-was-called partial-recall--reinforce)
      (bydi-was-called partial-recall--reclaim)
      (bydi-was-called partial-recall--forget)
      (bydi-was-called partial-recall--complete-dream)
      (bydi-was-called partial-recall--complete-reality)
      (bydi-was-called switch-to-buffer))))

;;; partial-recall-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

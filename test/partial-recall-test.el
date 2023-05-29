;;; partial-recall-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))

(ert-deftest pr--create-hash-key ()
  (with-mock ((random . (lambda () 42))
              (emacs-pid . (lambda () 1))
              (recent-keys . (lambda () 'keys))
              md5)

    (partial-recall--create-hash-key "test")
    (was-called-with md5 (list "test421keys"))))

(ert-deftest pr--on-create--sets-cdr ()
  (with-mock ((partial-recall--create-hash-key . (lambda (_) "test")))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab))))))

(ert-deftest pr--key--returns-if-set ()
  (with-mock ((tab-bar--current-tab . (lambda (&rest _) test-tab)))
    (should (string= "test-hash" (partial-recall--key)))))

(defmacro with-tab-history (&rest body)
  "Run BODY with a clear tab history and a temp buffer."
  (declare (indent 0))
  `(with-mock ((tab-bar--current-tab . (lambda (&rest _) test-tab)))
     (let ((partial-recall--table (make-hash-table)))
       (with-temp-buffer
         ,@body))))

(ert-deftest pr-remember--remembers ()
  (with-tab-history

   (partial-recall-remember)

   (should-not (null (gethash (alist-get 'pr test-tab) partial-recall--table)))))

(ert-deftest pr-remember--inserts-once ()
  (defvar partial-recall--table)

  (with-tab-history

    (partial-recall-remember)
    (partial-recall-remember)

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 1 (ring-length ring))))))

(ert-deftest pr-remember--extends-ring ()
  (with-tab-history
    (with-mock ((partial-recall--at-capacity . #'always)
                (partial-recall--should-extend-p . #'always)
                ring-extend)

      (partial-recall-remember (current-buffer))
      (was-called ring-extend))))

(ert-deftest pr-forget--forgets ()
  (with-tab-history
    (partial-recall-remember)

    (partial-recall-forget)

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 0 (ring-length ring))))))

(ert-deftest pr--on-close ()
  (with-tab-history
    (partial-recall-remember)

    (should (eq 1 (length (hash-table-keys partial-recall--table))))

    (partial-recall--on-close test-tab t)

    (should (eq 1 (length (hash-table-keys partial-recall--table))))

    (partial-recall--on-close test-tab nil)

    (should (eq 0 (length (hash-table-keys partial-recall--table))))))

(ert-deftest pr--history ()
  (with-tab-history
    (should-not (partial-recall--moments))

    (partial-recall-remember)

    (should (partial-recall--moments))))

(ert-deftest pr--current-p ()
  (with-tab-history
    (partial-recall-remember)
    (should (partial-recall--current-p (current-buffer)))))

(ert-deftest pr--has-buffers-p ()
  (with-tab-history
    (should-not (partial-recall--has-buffers-p))

    (partial-recall-remember)

    (should (partial-recall--has-buffers-p))))

(ert-deftest pr--known-buffer-p ()
  (with-tab-history
    (partial-recall-remember)

    (should (partial-recall--known-buffer-p (current-buffer)))))

(ert-deftest pr--memory-buffer-p ()
  (with-tab-history
    (partial-recall-remember)

    (let ((memory (gethash (alist-get 'pr test-tab) partial-recall--table)))

      (should (partial-recall--memory-buffer-p memory (current-buffer))))))

(ert-deftest pr-moment-buffer-p ()
  (with-tab-history
    (partial-recall-remember)

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory))
           (moment (ring-ref ring 0)))

      (should (partial-recall--moment-buffer-p moment (current-buffer))))))

(ert-deftest pr--maybe-remember ()
  (with-tab-history
    (with-mock (partial-recall-remember
                (buffer-live-p . #'always)
                (partial-recall--known-buffer-p . #'ignore))

      (partial-recall--maybe-remember (current-buffer))
      (was-called partial-recall-remember))))

(ert-deftest pr--maybe-remember--reclaims ()
  (with-tab-history
    (with-mock (partial-recall-remember
                partial-recall-reclaim
                (buffer-live-p . #'always)
                (partial-recall--known-buffer-p . #'always))

      (partial-recall--maybe-remember (current-buffer))
      (was-called partial-recall-reclaim))))

(ert-deftest pr-reclaim--reclaims-from-other ()
  (let ((seconds '(10 12))
        (partial-recall-reclaim-threshold 0))
    (with-tab-history
      (with-mock ((time-to-seconds . (lambda (&rest _) (pop seconds))))
        (partial-recall-remember)

        (with-mock ((partial-recall--current . (lambda () 'other))
                    partial-recall-remember)

          (partial-recall-reclaim nil (current-buffer))

          (was-called partial-recall-remember))))))

(ert-deftest pr-reclaim--no-op-for-same ()
  (with-tab-history
    (partial-recall-remember)

    (with-mock (partial-recall-remember)

      (with-current-buffer (current-buffer)
        (partial-recall-reclaim))

      (was-not-called partial-recall-remember))))

(ert-deftest pr--should-extend-p ()
  (let ((seconds '(10 11 12))
        (partial-recall-limit 1)
        (partial-recall-threshold 2))

    (with-tab-history
      (with-mock ((time-to-seconds . (lambda (&rest _) (pop seconds))))

        (partial-recall-remember)

        (let ((memory (partial-recall--get-or-create-memory (partial-recall--key))))

          (should (partial-recall--should-extend-p memory))
          (should-not (partial-recall--should-extend-p memory)))))))

(ert-deftest pr-recall--owner ()
  (with-tab-history
    (partial-recall-remember)

    (let ((memory (partial-recall--get-or-create-memory (partial-recall--key))))

      (should (eq memory (partial-recall--owner))))))

(ert-deftest pr--on-buffer-list-update--cancels-running-timer ()
  (let ((partial-recall--timer nil))

    (with-mock ((buffer-file-name . #'always)
                (partial-recall--known-buffer-p . #'ignore)
                cancel-timer
                run-at-time)

      (setq partial-recall--timer 'timer)
      (partial-recall--on-buffer-list-update)

      (was-called cancel-timer)

      (was-called run-at-time))))

(ert-deftest pr--on-frame-delete ()
  (defvar tab-bar-tabs-function nil)

  (let ((tab-bar-tabs-function (lambda (_) '(one two))))
    (with-mock (partial-recall--on-close)

      (partial-recall--on-frame-delete 'frame)

      (was-called-n-times partial-recall--on-close 2))))

(ert-deftest pr--setup ()
  (defvar tab-bar-tabs-function nil)
  (defvar tab-bar-mode nil)

  (let ((tab-bar-tabs-function (lambda () (list test-tab)))
        (tab-bar-mode nil))

    (with-mock (add-hook
                (partial-recall--key . #'ignore)
                partial-recall--on-create
                (tab-bar-mode . (lambda (_) (setq tab-bar-mode t))))

      (partial-recall-mode--setup)

      (was-called partial-recall--on-create)
      (was-called-n-times add-hook 5)
      (was-called tab-bar-mode))))

(ert-deftest pr--setup--messages-on-fail ()
  (defvar tab-bar-tabs-function nil)
  (defvar tab-bar-mode nil)

  (let ((tab-bar-tabs-function (lambda () (list test-tab)))
        (tab-bar-mode nil))

    (with-mock (add-hook
                (partial-recall--key . #'ignore)
                partial-recall--on-create
                tab-bar-mode
                message)

      (partial-recall-mode--setup)

      (was-not-called partial-recall--on-create)
      (was-called-n-times add-hook 5)
      (was-called message))))

(ert-deftest pr--teardown ()
  (with-mock (remove-hook)

      (partial-recall-mode--teardown)

      (was-called-n-times remove-hook 5)))

(ert-deftest pr-mode ()
  (with-mock (partial-recall-mode--setup
              partial-recall-mode--teardown)

    (partial-recall-mode 1)

    (was-called partial-recall-mode--setup)

    (partial-recall-mode -1)

    (was-called partial-recall-mode--teardown)))

;;; partial-recall-test.el ends here

;;; partial-recall-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))

(ert-deftest pr--create-hash-key ()
  (bydi-with-mock ((:mock random :return 42)
                   (:mock emacs-pid :return 1)
                   (:mock recent-keys :return 'keys)
                   md5)

    (partial-recall--create-hash-key "test")
    (bydi-was-called-with md5 "test421keys")))

(ert-deftest pr--on-create--sets-cdr ()
  (bydi-with-mock ((:mock partial-recall--create-hash-key :return "test"))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab))))))

(ert-deftest pr--key--returns-if-set ()
  (bydi-with-mock ((:mock tab-bar--current-tab :return test-tab))
    (should (string= "test-hash" (partial-recall--key)))))

(defmacro with-tab-history (&rest body)
  "Run BODY with a clear tab history and a temp buffer."
  (declare (indent 0))
  `(bydi-with-mock ((:mock tab-bar--current-tab :return test-tab))
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
    (bydi-with-mock ((partial-recall--at-capacity . #'always)
                     (partial-recall--should-extend-p . #'always)
                     ring-extend)

      (partial-recall-remember (current-buffer))
      (bydi-was-called ring-extend))))

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
    (bydi-with-mock (partial-recall-remember
                     (buffer-live-p . #'always)
                     (partial-recall--known-buffer-p . #'ignore))

      (partial-recall--maybe-remember (current-buffer))
      (bydi-was-called partial-recall-remember))))

(ert-deftest pr--maybe-remember--reclaims ()
  (with-tab-history
    (bydi-with-mock (partial-recall-remember
                     partial-recall-reclaim
                     (buffer-live-p . #'always)
                     (partial-recall--known-buffer-p . #'always))

      (partial-recall--maybe-remember (current-buffer))
      (bydi-was-called partial-recall-reclaim))))

(ert-deftest pr-reclaim--reclaims-from-other ()
  (let ((seconds '(10 12))
        (partial-recall-reclaim-threshold 0))
    (with-tab-history
      (bydi-with-mock ((time-to-seconds . (lambda (&rest _) (pop seconds))))
        (partial-recall-remember)

        (bydi-with-mock ((:mock partial-recall--current :return 'other)
                         partial-recall-remember)

          (partial-recall-reclaim nil (current-buffer))

          (bydi-was-called partial-recall-remember))))))

(ert-deftest pr-reclaim--no-op-for-same ()
  (with-tab-history
    (partial-recall-remember)

    (bydi-with-mock (partial-recall-remember)

      (with-current-buffer (current-buffer)
        (partial-recall-reclaim))

      (bydi-was-not-called partial-recall-remember))))

(ert-deftest pr--should-extend-p ()
  (let ((seconds '(10 11 12))
        (partial-recall-limit 1)
        (partial-recall-threshold 2))

    (with-tab-history
      (bydi-with-mock ((time-to-seconds . (lambda (&rest _) (pop seconds))))

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

    (bydi-with-mock ((buffer-file-name . #'always)
                     (partial-recall--known-buffer-p . #'ignore)
                     cancel-timer
                     run-at-time)

      (setq partial-recall--timer 'timer)
      (partial-recall--on-buffer-list-update)

      (bydi-was-called cancel-timer)

      (bydi-was-called run-at-time))))

(ert-deftest pr--on-frame-delete ()
  (defvar tab-bar-tabs-function nil)

  (let ((tab-bar-tabs-function (lambda (_) '(one two))))
    (bydi-with-mock (partial-recall--on-close)

      (partial-recall--on-frame-delete 'frame)

      (bydi-was-called-n-times partial-recall--on-close 2))))

(ert-deftest pr--setup ()
  (defvar tab-bar-tabs-function nil)
  (defvar tab-bar-mode nil)

  (let ((tab-bar-tabs-function (lambda () (list test-tab)))
        (tab-bar-mode nil))

    (bydi-with-mock (add-hook
                     (partial-recall--key . #'ignore)
                     partial-recall--on-create
                     (tab-bar-mode . (lambda (_) (setq tab-bar-mode t))))

      (partial-recall-mode--setup)

      (bydi-was-called partial-recall--on-create)
      (bydi-was-called-n-times add-hook 5)
      (bydi-was-called tab-bar-mode))))

(ert-deftest pr--setup--messages-on-fail ()
  (defvar tab-bar-tabs-function nil)
  (defvar tab-bar-mode nil)

  (let ((tab-bar-tabs-function (lambda () (list test-tab)))
        (tab-bar-mode nil))

    (bydi-with-mock (add-hook
                     (partial-recall--key . #'ignore)
                     partial-recall--on-create
                     tab-bar-mode
                     message)

      (partial-recall-mode--setup)

      (bydi-was-not-called partial-recall--on-create)
      (bydi-was-called-n-times add-hook 5)
      (bydi-was-called message))))

(ert-deftest pr--teardown ()
  (bydi-with-mock (remove-hook)

    (partial-recall-mode--teardown)

    (bydi-was-called-n-times remove-hook 5)))

(ert-deftest pr-mode ()
  (bydi-with-mock (partial-recall-mode--setup
                   partial-recall-mode--teardown)

    (partial-recall-mode 1)

    (bydi-was-called partial-recall-mode--setup)

    (partial-recall-mode -1)

    (bydi-was-called partial-recall-mode--teardown)))

;;; partial-recall-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

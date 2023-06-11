;;; partial-recall-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

(defvar test-tab '(current-tab (name . "test-tab") (explicit-name . t) (pr . "test-hash")))

(ert-deftest pr--create-hash-key ()
  (bydi ((:mock random :return 42)
         (:mock emacs-pid :return 1)
         (:mock recent-keys :return 'keys)
         md5)

    (partial-recall--create-key "test")
    (bydi-was-called-with md5 "test421keys")))

(ert-deftest pr--on-create--sets-cdr ()
  (bydi ((:mock partial-recall--create-key :return "test"))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab))))))

(ert-deftest pr--key--returns-if-set ()
  (bydi ((:mock tab-bar--current-tab :return test-tab))
    (should (string= "test-hash" (partial-recall--key)))))

(defmacro with-tab-history (&rest body)
  "Run BODY with a clear tab history and a temp buffer."
  (declare (indent 0))
  `(bydi ((:mock tab-bar--current-tab :return test-tab))
     (let ((partial-recall--table (make-hash-table)))
       (with-temp-buffer
         ,@body))))

(ert-deftest pr-remember--remembers ()
  (with-tab-history

   (partial-recall--remember)

   (should-not (null (gethash (alist-get 'pr test-tab) partial-recall--table)))))

(ert-deftest pr-remember--inserts-once ()
  (defvar partial-recall--table)

  (with-tab-history

   (partial-recall--remember)
   (partial-recall--remember)

   (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
          (ring (partial-recall--memory-ring memory)))

     (should (eq 1 (ring-length ring))))))

(ert-deftest pr-remember--extends-ring ()
  (with-tab-history
   (bydi ((:always partial-recall--memory-at-capacity-p)
          (:always partial-recall--should-extend-memory-p)
          ring-extend)

     (partial-recall--remember)
     (bydi-was-called ring-extend))))

(ert-deftest pr-forget--forgets ()
  (with-tab-history
   (partial-recall--remember)

   (partial-recall--forget)

   (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
          (ring (partial-recall--memory-ring memory)))

     (should (eq 0 (ring-length ring))))))

(ert-deftest pr--on-close ()
  (with-tab-history
   (partial-recall--remember)

   (should (eq 1 (length (hash-table-keys partial-recall--table))))

   (partial-recall--on-close test-tab t)

   (should (eq 1 (length (hash-table-keys partial-recall--table))))

   (partial-recall--on-close test-tab nil)

   (should (eq 0 (length (hash-table-keys partial-recall--table))))))

(ert-deftest pr--history ()
  (with-tab-history
   (should-not (partial-recall--moments))

   (partial-recall--remember)

   (should (partial-recall--moments))))

(ert-deftest pr--current-p ()
  (with-tab-history
   (partial-recall--remember)
   (should (partial-recall--reality-buffer-p (current-buffer)))))

(ert-deftest pr--has-buffers-p ()
  (with-tab-history
   (should-not (partial-recall--has-buffers-p))

   (partial-recall--remember)

   (should (partial-recall--has-buffers-p))))

(ert-deftest pr--mapped-buffer-p ()
  (with-tab-history
   (partial-recall--remember)

   (should (partial-recall--mapped-buffer-p (current-buffer)))))

(ert-deftest pr--memory-buffer-p ()
  (with-tab-history
   (partial-recall--remember)

   (let ((memory (gethash (alist-get 'pr test-tab) partial-recall--table)))

     (should (partial-recall--memory-buffer-p memory (current-buffer))))))

(ert-deftest pr-moment-buffer-p ()
  (with-tab-history
   (partial-recall--remember)

   (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
          (ring (partial-recall--memory-ring memory))
          (moment (ring-ref ring 0)))

     (should (partial-recall--moment-buffer-p moment (current-buffer))))))

(ert-deftest pr--maybe-remember ()
  (with-tab-history
   (bydi (partial-recall--remember
          (:always buffer-live-p)
          (:ignore partial-recall--mapped-buffer-p))

     (partial-recall--handle-buffer (current-buffer))
     (bydi-was-called partial-recall--remember))))

(ert-deftest pr--maybe-remember--reclaims ()
  (with-tab-history
   (bydi (partial-recall--remember
          partial-recall--reclaim
          (:always buffer-live-p)
          (:always partial-recall--mapped-buffer-p))

     (partial-recall--handle-buffer (current-buffer))
     (bydi-was-called partial-recall--reclaim))))

(ert-deftest pr-reclaim--reclaims-from-other ()
  (let ((seconds '(10 12))
        (partial-recall-reclaim-min-age 0))
    (with-tab-history
     (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))
       (partial-recall--remember)

       (bydi ((:mock partial-recall--reality :return 'other)
              partial-recall--remember)

         (partial-recall--reclaim)

         (bydi-was-called partial-recall--remember))))))

(ert-deftest pr-reclaim--no-op-for-same ()
  (with-tab-history
   (partial-recall--remember)

   (bydi (partial-recall--remember)

     (with-current-buffer (current-buffer)
       (partial-recall--reclaim))

     (bydi-was-not-called partial-recall--remember))))

(ert-deftest pr--should-extend-p ()
  (let ((seconds '(10 11 12))
        (partial-recall-limit 1)
        (partial-recall-max-age 2))

    (with-tab-history
     (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

       (partial-recall--remember)

       (let ((memory (partial-recall--get-or-create-memory (partial-recall--key))))

         (should (partial-recall--should-extend-memory-p memory))
         (should-not (partial-recall--should-extend-memory-p memory)))))))

(ert-deftest pr-recall--owner ()
  (with-tab-history
   (partial-recall--remember)

   (let ((memory (partial-recall--get-or-create-memory (partial-recall--key))))

     (should (eq memory (partial-recall--buffer-owner))))))

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
    (bydi (partial-recall--on-close)

      (partial-recall--on-frame-delete 'frame)

      (bydi-was-called-n-times partial-recall--on-close 2))))

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

(ert-deftest pr-mode ()
  (bydi (partial-recall-mode--setup
         partial-recall-mode--teardown)

    (partial-recall-mode 1)

    (bydi-was-called partial-recall-mode--setup)

    (partial-recall-mode -1)

    (bydi-was-called partial-recall-mode--teardown)))

(ert-deftest pr--api ()
  (let ((partial-recall-mode nil))

    (bydi (partial-recall--remember
           partial-recall--reclaim
           partial-recall--forget
           partial-recall--complete-dream
           partial-recall--complete-reality
           switch-to-buffer)

      (partial-recall-remember)
      (partial-recall-reclaim)
      (partial-recall-forget)
      (call-interactively 'partial-recall-steal)
      (call-interactively 'partial-recall-switch-to-buffer)

      (bydi-was-called partial-recall--remember)
      (bydi-was-called partial-recall--reclaim)
      (bydi-was-called partial-recall--forget)
      (bydi-was-called partial-recall--complete-dream)
      (bydi-was-called partial-recall--complete-reality)
      (bydi-was-called switch-to-buffer))))

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

(ert-deftest pr--owns ()
  (with-tab-history
    (partial-recall--remember)
    (should (partial-recall--reality-owns-buffer-p (current-buffer)))))

(ert-deftest pr--warn ()
  (bydi (display-warning)
    (partial-recall--warn "Testing")
    (bydi-was-called-with display-warning (list 'partial-recall "Testing" :warning))))

;;; partial-recall-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

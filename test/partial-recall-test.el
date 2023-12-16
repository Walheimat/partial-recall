;;; partial-recall-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall nil t)

;;; -- Accessors

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
  :tags '(needs-history)
  (with-tab-history :pre t

    (let ((memory (partial-recall--reality)))

      (should (eq memory (partial-recall--buffer-owner))))))

(ert-deftest pr--focus ()
  :tags '(needs-history)
  (let ((partial-recall-memory-size 2))

    (with-tab-history :pre t

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (partial-recall--reinforce (current-buffer))

        (should (eq 1 (partial-recall--focus)))
        (kill-buffer another-temp)))))

(ert-deftest pr--name ()
  :tags '(needs-history)
  (with-tab-history
    (let ((reality (partial-recall--reality)))

      (bydi ((:sometimes partial-recall--subconscious-p))

        (should (string= partial-recall--subconscious-key (partial-recall--name reality)))

        (bydi-toggle-sometimes)

        (should (string= "test-tab" (partial-recall--name reality)))))))

(ert-deftest pr--tab ()
  :tags '(needs-history)
  (with-tab-history :pre t
    (should (string= (partial-recall--tab-name nil (selected-frame))
                     "test-tab"))))

(ert-deftest pr--ensure-subconscious ()
  (let ((partial-recall-table (make-hash-table)))

    (partial-recall--subconscious)

    (should (gethash partial-recall--subconscious-key partial-recall--table))))

;;; -- Handlers

(ert-deftest pr--schedule-buffer--cancels-running-timer ()
  (let ((partial-recall--timer nil)
        (partial-recall-handle-delay 1)
        (buffer (current-buffer))
        (timer (timer--create)))

    (bydi ((:always buffer-file-name)
           (:ignore partial-recall--mapped-buffer-p)
           partial-recall--void-timer
           run-at-time)

      (partial-recall--schedule-buffer buffer)

      (bydi-was-called partial-recall--void-timer)
      (bydi-was-called-with run-at-time `(1 nil partial-recall--handle-buffer ,buffer)))))

(ert-deftest pr--schedule-buffer--ignores-neglected ()
  (with-temp-buffer
    (setq partial-recall--neglect (current-buffer))
    (should-not (partial-recall--schedule-buffer (current-buffer)))
    (should-not partial-recall--neglect)))

(ert-deftest pr--handle-buffer ()
  :tags '(needs-history)
  (with-tab-history
    (let ((buffers (list (current-buffer))))

      (bydi (partial-recall--remember
             (:always buffer-live-p)
             (:mock partial-recall--window-list :return buffers)
             (:mock window-buffer :with bydi-rf)
             (:ignore partial-recall--mapped-buffer-p))

        (partial-recall--handle-buffer (current-buffer))
        (bydi-was-called partial-recall--remember)
        (bydi-was-called partial-recall--window-list)))))

(ert-deftest pr--handle-buffer--missing ()
  :tags '(needs-history)
  (with-tab-history
    (let ((buffers nil))

      (bydi (partial-recall--remember
             (:always buffer-live-p)
             (:mock window-list :return buffers)
             (:mock window-buffer :with bydi-rf)
             (:ignore partial-recall--mapped-buffer-p))

        (partial-recall--handle-buffer (current-buffer))
        (bydi-was-not-called partial-recall--remember)
        (bydi-was-called window-list)))))

(ert-deftest pr--handle-buffer--reclaims ()
  :tags '(needs-history)
  (with-tab-history
    (bydi (partial-recall--remember
           partial-recall--recollect
           (:always buffer-live-p)
           (:mock window-buffer :return (current-buffer))
           (:always partial-recall--mapped-buffer-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--recollect))))

(ert-deftest pr--void-timer--voids ()
  (let* ((timer (timer--create))
         (partial-recall--timer timer))

    (setf (timer--triggered timer) nil)

    (bydi (cancel-timer)

      (partial-recall--void-timer)

      (bydi-was-called cancel-timer)

      (should-not partial-recall--timer))))

(ert-deftest pr--concentrate--does-nothing-for-non-meaningful ()
  (let ((partial-recall--last-focus nil))

    (bydi ((:ignore partial-recall--meaningful-buffer-p))

      (partial-recall--concentrate)

      (should-not partial-recall--last-focus))))

(ert-deftest pr--concentrate ()
  (let ((partial-recall--last-focus 'last))

    (bydi ((:sometimes partial-recall--can-hold-concentration-p)
           partial-recall--intensify
           partial-recall--debug
           (:mock partial-recall--moment-from-buffer :return 'next))

      (partial-recall--concentrate)

      (bydi-was-called partial-recall--intensify)

      (bydi-toggle-sometimes)

      (partial-recall--concentrate)

      (bydi-was-called partial-recall--debug)

      (should (eq 'next partial-recall--last-focus)))))

(ert-deftest pr--can-hold-concentration-p ()
  :tags '(needs-history)
  (with-tab-history :pre t
    (let ((partial-recall--last-focus nil))

      (should-not (partial-recall--can-hold-concentration-p))

      (setq partial-recall--last-focus (partial-recall--moment-from-buffer (current-buffer)))

      (should (partial-recall--can-hold-concentration-p))

      (bydi ((:sometimes partial-recall--buffer-remains-visible-p))
        (let* ((another (generate-new-buffer " *temp*" t))
               (moment (partial-recall--moment-create another)))

          (setq partial-recall--last-focus moment)
          (should-not (partial-recall--can-hold-concentration-p)))))))

(ert-deftest pr--explain-omission-internal ()
  :tags '(meaningful)

  (defun test-meaningful (buffer)
    "Test trait for BUFFER."
    nil)

  (put 'test-meaningful 'partial-recall-non-meaningful-explainer "Testing")

  (let ((partial-recall-meaningful-traits '(test-meaningful)))

    (should (string= "Testing" (partial-recall--explain-omission)))))

(ert-deftest pr--after-switch-to-buffer--schedules ()
  (bydi (partial-recall--schedule-buffer)

    (partial-recall--after-switch-to-buffer (current-buffer) t)

    (bydi-was-not-called partial-recall--schedule-buffer)

    (partial-recall--after-switch-to-buffer (current-buffer))

    (bydi-was-called partial-recall--schedule-buffer)))

(ert-deftest pr--before-switch-to-buffer--maybe-switches ()
  (bydi (partial-recall--maybe-switch-memory)

    (partial-recall--before-switch-to-buffer (current-buffer) t)

    (bydi-was-not-called partial-recall--maybe-switch-memory)

    (partial-recall--before-switch-to-buffer (current-buffer))

    (bydi-was-called partial-recall--maybe-switch-memory)))

(ert-deftest pr--after-pop-to-buffer--schedules ()
  (let ((buffer (current-buffer)))

    (bydi (partial-recall--schedule-buffer)

      (partial-recall--after-pop-to-buffer buffer)

      (bydi-was-called-with partial-recall--schedule-buffer (current-buffer)))))

(ert-deftest pr--on-create--sets-cdr ()
  (bydi ((:mock partial-recall--create-key :return "test"))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab))))))

(ert-deftest pr--on-close ()
  :tags '(needs-history)
  (bydi (partial-recall--suppress)
    (with-tab-history :pre t

      (should (eq 1 (length (hash-table-keys partial-recall--table))))

      (partial-recall--on-close test-tab t)

      (should (eq 1 (length (hash-table-keys partial-recall--table))))

      (partial-recall--on-close test-tab nil)

      (should (eq 0 (length (hash-table-keys partial-recall--table))))

      (bydi-was-called partial-recall--suppress))))

(ert-deftest pr--on-frame-delete ()
  (defvar tab-bar-tabs-function nil)

  (let ((tab-bar-tabs-function (lambda (_) '(one two))))
    (bydi (partial-recall--on-close
           tab-bar-tabs)

      (partial-recall--on-frame-delete 'frame)

      (bydi-was-called-n-times partial-recall--on-close 2))))

(ert-deftest pr--on-minibuffer-setup--records-for-triggers ()
  (let ((partial-recall-record-triggers '(a b c))
        (this-command 'b))

    (bydi (minibuffer-selected-window
           (:mock window-buffer :return 'mini))

      (partial-recall--on-minibuffer-setup)

      (bydi-was-called minibuffer-selected-window)
      (should (eq 'mini partial-recall--before-minibuffer))

      (setq this-command 'd
            partial-recall--before-minibuffer nil)
      (bydi-clear-mocks)

      (partial-recall--on-minibuffer-setup)

      (bydi-was-not-called minibuffer-selected-window)
      (should-not partial-recall--before-minibuffer))))

(ert-deftest pr--on-minibuffer-setup--deletes-recording ()
  (let ((partial-recall--before-minibuffer 'something))

    (partial-recall--on-minibuffer-exit)

    (should-not partial-recall--before-minibuffer)))

;;; -- Actions

(ert-deftest pr-remember--remembers ()
  :tags '(needs-history)
  (with-tab-history

    (partial-recall--remember (current-buffer))

    (should-not (null (gethash (alist-get 'pr test-tab) partial-recall--table)))))

(ert-deftest pr-remember--inserts-once ()
  :tags '(needs-history)

  (defvar partial-recall--table)

  (with-tab-history :pre t

    (partial-recall--remember (current-buffer))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 1 (ring-length ring))))))

(ert-deftest pr-remember--extends-ring ()
  :tags '(needs-history)

  (with-tab-history :probes t
    (bydi ((:always partial-recall--memory-at-capacity-p)
           (:always partial-recall--should-extend-memory-p)
           partial-recall--maybe-reinserted-implanted
           partial-recall--maybe-suppress-oldest-moment
           partial-recall--maybe-forget-oldest-moment
           ring-extend)

      (partial-recall--remember (current-buffer))
      (bydi-was-called ring-extend))))

(ert-deftest pr-remember--reinforces-permanent ()
  :tags '(needs-history)

  (let ((partial-recall-memory-size 1))

    (with-tab-history :pre t :probes t
      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--implant)

        (bydi ((:spy partial-recall--reinsert))
          (partial-recall--remember another-temp)

          (bydi-was-called partial-recall--reinsert))

        (kill-buffer another-temp)))))

(ert-deftest pr-remember--removes-impermanent ()
  :tags '(needs-history)

  (let ((partial-recall-memory-size 1))
    (with-tab-history :pre t :probes t
      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (bydi ((:always partial-recall--memory-at-capacity-p)
               (:ignore partial-recall--should-extend-memory-p)
               partial-recall--maybe-resize-memory
               partial-recall--maybe-extend-memory
               partial-recall--maybe-reinsert-implanted
               partial-recall--suppress)
          (partial-recall--remember another-temp)

          (bydi-was-called partial-recall--suppress))

        (kill-buffer another-temp)))))

(ert-deftest pr-swap ()
  (let* ((partial-recall-memory-size 1)
         (a (partial-recall--memory-create "a"))
         (b (partial-recall--memory-create "b"))
         (buffer (generate-new-buffer " *temp*" t))
         (moment (partial-recall--moment-create buffer)))

    (ring-insert (partial-recall--memory-ring a) moment)

    (bydi ((:mock partial-recall--tab-name :return "tab")
           (:mock buffer-name :return "buffer")
           partial-recall--maybe-reinsert-implanted
           partial-recall--moment-update-timestamp
           (:always partial-recall--memory-at-capacity-p)
           (:always partial-recall--should-extend-memory-p))

      (partial-recall--swap a b moment)

      (bydi-was-called partial-recall--moment-update-timestamp)

      (let ((ring (partial-recall--memory-ring b)))

        (should (eq 2 (ring-size ring)))
        (should (ring-member ring moment))))))

(ert-deftest pr-recollect--reinforces-reality-or-reclaims ()
  (bydi ((:sometimes partial-recall--memory-buffer-p)
         partial-recall--reinforce
         partial-recall--reclaim)
    (partial-recall--recollect (current-buffer))

    (bydi-was-called partial-recall--reinforce)
    (bydi-toggle-sometimes)

    (let ((partial-recall-reclaim t))

      (partial-recall--recollect (current-buffer))
      (bydi-was-called partial-recall--reclaim)

      (bydi-clear-mocks)

      (setq partial-recall-reclaim nil)
      (partial-recall--recollect (current-buffer))
      (bydi-was-not-called partial-recall--reclaim))))

(ert-deftest pr-reinforce--reinforces-old-buffers ()
  :tags '(needs-history)

  (with-tab-history
    (let ((count nil)
          (seconds '(6 8 10 12))
          (get-count (lambda ()
                       (let* ((reality (partial-recall--reality))
                              (moments (partial-recall--memory-ring reality))
                              (buffer (current-buffer))
                              (moment (ring-ref moments (partial-recall--moments-member moments buffer))))
                         (partial-recall--moment-focus moment))))
          (partial-recall-memory-size 2)
          (another-temp (generate-new-buffer " *temp*" t)))

      (bydi ((:mock time-to-seconds :with (lambda () (pop seconds))))
        (partial-recall--remember (current-buffer))

        (setq count (funcall get-count))

        (partial-recall--remember another-temp)

        (should (partial-recall--reinforce (current-buffer)))
        (should-not (eq count (funcall get-count))))

      (kill-buffer another-temp))))

(ert-deftest pr-reclaim--reclaims-from-other ()
  :tags '(needs-history)

  (let ((seconds '(10 12))
        (partial-recall-reclaim-min-age -1)
        (mock-reality (partial-recall--memory-create "other-key")))

    (with-tab-history
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))
        (partial-recall--remember (current-buffer))

        (bydi ((:mock partial-recall--reality :return mock-reality)
               partial-recall--swap)

          (let ((moment (partial-recall--moment-from-buffer (current-buffer))))

            (partial-recall--moment-set-permanence moment t)

            (partial-recall--reclaim (current-buffer))

            (bydi-was-not-called partial-recall--swap)

            (partial-recall--moment-set-permanence moment nil)

            (partial-recall--reclaim (current-buffer))

            (bydi-was-called partial-recall--swap)))))))

(ert-deftest pr-reclaim--no-op-for-same ()
  :tags '(needs-history)

  (with-tab-history :pre t

    (bydi (partial-recall--remember)

      (partial-recall--reclaim (current-buffer))

      (bydi-was-not-called partial-recall--remember))))

(ert-deftest pr-forget--forgets ()
  :tags '(needs-history)

  (with-tab-history :pre t

    (bydi (partial-recall--suppress)
      (partial-recall--forget (current-buffer) t)

      (bydi-was-called partial-recall--suppress))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 0 (ring-length ring))))))

(ert-deftest pr--forget-some ()
  :tags '(needs-history)

  (with-tab-history :pre t

    (bydi ((:always yes-or-no-p)
           partial-recall--forget
           (:mock partial-recall--repr :return "#<test-moment>"))

      (partial-recall--forget-some)

      (bydi-was-called-with yes-or-no-p "Forget #<test-moment> (unmodified)?")

      (bydi-was-called-with partial-recall--forget (list (current-buffer) t)))))

(ert-deftest pr-forget--clears-subconscious ()
  :tags '(needs-history)

  (with-tab-history :pre t :lifts t

    (let ((sub (partial-recall--subconscious)))

      (partial-recall--forget (current-buffer) t)

      (should (partial-recall--memory-buffer-p (current-buffer) sub))

      (partial-recall--forget (current-buffer))

      (should-not (partial-recall--memory-buffer-p (current-buffer) sub)))))

(ert-deftest pr-forget--shortens-extended-memory ()
  :tags '(needs-history)

  (let ((partial-recall-memory-size 2)
        (another-temp (generate-new-buffer " *temp*" t))
        (yet-another-temp (generate-new-buffer " *temp*" t))
        (partial-recall-reclaim-min-age -1))

    (bydi (partial-recall--suppress
           partial-recall--maybe-reinsert-implanted)
      (with-tab-history :pre t :probes t
        (let ((ring (partial-recall--memory-ring (partial-recall--reality))))

          (partial-recall--remember another-temp)
          (partial-recall--remember yet-another-temp)

          (should (eq (ring-size ring) 3))
          (should (eq (ring-length ring) 3))

          (partial-recall--flush (partial-recall--reality))

          (should (eq (ring-size ring) 2))
          (should (eq (ring-length ring) 0))

          (kill-buffer another-temp)
          (kill-buffer yet-another-temp))))))

(ert-deftest partial-recall--suppress--remembers-unique ()
  :tags '(needs-history)

  (with-tab-history :pre t
    (partial-recall--forget (current-buffer) t)
    (partial-recall--remember (current-buffer))
    (partial-recall--forget (current-buffer) t)

    (should (eq (ring-length (partial-recall--memory-ring (gethash partial-recall--subconscious-key partial-recall--table)))
                1))))

(ert-deftest partial-recall--suppress--removes-permanence ()
  :tags '(needs-history)

  (with-tab-history :pre t
    (let ((moment (partial-recall--moment-from-buffer (current-buffer))))

      (partial-recall--implant (current-buffer))
      (should (partial-recall--moment-permanence moment))

      (partial-recall--forget (current-buffer) t)
      (should-not (partial-recall--moment-permanence moment)))))

(ert-deftest partial-recall--suppress--kills ()
  :tags '(needs-history)

  (with-tab-history

    (let ((another-temp (generate-new-buffer " *temp*" t)))

      (partial-recall--remember another-temp)
      (partial-recall--remember (current-buffer))

      (partial-recall--forget another-temp t)

      (bydi ((:always partial-recall--memory-at-capacity-p)
             kill-buffer)
        (partial-recall--forget (current-buffer) t)

        (bydi-was-called-with kill-buffer another-temp))

      (kill-buffer another-temp))))

(ert-deftest partial-recall--suppress--just-removes ()
  :tags '(needs-history)

  (let ((partial-recall-repress nil))

    (with-tab-history

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (partial-recall--remember (current-buffer))
        (partial-recall--forget another-temp)

        (bydi ((:always partial-recall--memory-at-capacity-p)
               kill-buffer)
          (partial-recall--forget)

          (bydi-was-not-called kill-buffer))

        (kill-buffer another-temp)))))

(ert-deftest partial-recall--lift ()
  :tags '(needs-history)

  (with-tab-history :lifts t :pre t
    (let ((sub (gethash partial-recall--subconscious-key partial-recall--table)))

      (partial-recall--forget (current-buffer) t)

      (should (eq (ring-length (partial-recall--memory-ring sub)) 1))

      (partial-recall--lift (current-buffer))

      (should (eq (ring-length (partial-recall--memory-ring sub)) 0)))))

(ert-deftest pr--maybe-implant-moment ()
  :tags '(needs-history)

  (let ((partial-recall-auto-implant t)
        (partial-recall-auto-implant-threshold 1))
    (with-tab-history :pre t

      (bydi ((:spy partial-recall--maybe-implant-moment)
             (:spy partial-recall--implant)
             partial-recall--reset-count)

        (partial-recall--reinforce (current-buffer))
        (partial-recall--reinforce (current-buffer))
        (partial-recall--reinforce (current-buffer))

        (bydi-was-called-n-times partial-recall--maybe-implant-moment 2)
        (bydi-was-called-n-times partial-recall--implant 1)

        (let ((moment (partial-recall--moment-from-buffer (current-buffer))))
          (should (partial-recall--moment-permanence moment)))

        (partial-recall--implant (current-buffer) t)

        (bydi-was-called partial-recall--reset-count)))))

(ert-deftest pr--maybe-switch-memory ()
  :tags '(needs-history)

  (let ((partial-recall-auto-switch 'prompt)
        (user-input nil))

    (with-tab-history :pre t :second t
      (let* ((another (generate-new-buffer " *temp*" t))
             (moment (partial-recall--moment-create another)))

        (ring-insert (partial-recall--memory-ring second-memory) moment)

        (should-not (partial-recall--maybe-switch-memory (current-buffer)))

        (bydi (tab-bar-switch-to-tab
               (:mock partial-recall--tab-name :return "test")
               (:mock yes-or-no-p :return user-input))

          (partial-recall--maybe-switch-memory another)
          (bydi-was-not-called tab-bar-switch-to-tab)

          (bydi-clear-mocks)
          (setq user-input t)

          (partial-recall--maybe-switch-memory another)
          (bydi-was-called tab-bar-switch-to-tab)

          (bydi-clear-mocks)
          (setq partial-recall-auto-switch nil)

          (partial-recall--maybe-switch-memory another)
          (bydi-was-not-called tab-bar-switch-to-tab)

          (bydi-clear-mocks)
          (setq partial-recall-auto-switch t)
          (partial-recall--maybe-switch-memory another)

          (bydi-was-called-with tab-bar-switch-to-tab "test"))))))

(ert-deftest pr--clean-up-buffer ()
  (let ((partial-recall--last-checked (current-buffer)))

    (partial-recall--clean-up-buffer (current-buffer))

    (should-not partial-recall--last-checked))

  (bydi ((:mock window-buffer :return (current-buffer))
         quit-window)

    (partial-recall--clean-up-buffer (current-buffer))

    (bydi-was-called quit-window)))

(ert-deftest pr--clean-up-buffer--clears-focus ()
  :tags '(needs-history)

  (with-tab-history :pre t
    (bydi ((:mock window-buffer :return (current-buffer))
           quit-window)

      (should partial-recall--last-focus)

      (partial-recall--clean-up-buffer (current-buffer))

      (should-not partial-recall--last-focus))))

(ert-deftest pr--meld ()
  :tags '(needs-history)

  (with-tab-history :pre t :second t
    (should-error (partial-recall--meld (partial-recall--reality) (partial-recall--reality)))

    (bydi (tab-bar-close-tab-by-name)
      (partial-recall--meld (partial-recall--reality) second-memory t)

      (should (ring-empty-p (partial-recall--memory-ring (partial-recall--reality))))

      (should (partial-recall--memory-buffer-p (current-buffer) second-memory))

      (bydi-was-called-with tab-bar-close-tab-by-name "test-tab"))))

(ert-deftest pr--flush ()
  :tags '(needs-history)

  (with-tab-history :pre t
    (let ((another (generate-new-buffer " *temp*" t))
          (ring (partial-recall--memory-ring (partial-recall--reality)))
          (partial-recall-reclaim-min-age -1))

      (partial-recall--remember another)

      (should (eq 2 (ring-length ring)))

      (partial-recall--implant another)

      (bydi (partial-recall--clean-up-buffer)
        (partial-recall--flush (partial-recall--reality))

        (bydi-was-called partial-recall--clean-up-buffer))

      (should (eq 1 (ring-length ring)))
      (should (eq another (partial-recall--moment-buffer (ring-ref ring 0))))

      (with-current-buffer another
        (partial-recall--implant another t)

        (partial-recall--flush (partial-recall--reality) t)

        (should (eq 1 (ring-length ring)))
        (should (eq another (partial-recall--moment-buffer (ring-ref ring 0))))))))

(ert-deftest pr--switch-to-and-neglect ()
  (let ((partial-recall--switch-to-buffer-function 'switch-to-buffer))

    (bydi (switch-to-buffer)

      (with-temp-buffer
        (partial-recall--switch-to-and-neglect (current-buffer))

        (should (eq (current-buffer) partial-recall--neglect))

        (bydi-was-called-with switch-to-buffer (current-buffer))))))

(ert-deftest pr--next-and-prev ()
  :tags '(needs-history)

  (with-tab-history :pre t
    (let ((another (generate-new-buffer " *temp*" t)))

      (partial-recall--remember another)

      (should (equal (partial-recall--next-buffer) another))
      (should (equal (partial-recall--previous-buffer) another)))))

;;; -- Conditionals

(ert-deftest pr--memory-buffer-p ()
  :tags '(needs-history)

  (with-tab-history :pre t

    (let ((memory (gethash (alist-get 'pr test-tab) partial-recall--table)))

      (should (partial-recall--memory-buffer-p (current-buffer) memory)))))

(ert-deftest pr-moment-buffer-p ()
  :tags '(needs-history)

  (with-tab-history :pre t

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory))
           (moment (ring-ref ring 0)))

      (should (partial-recall--moment-buffer-p (current-buffer) moment)))))

(ert-deftest pr--mapped-buffer-p ()
  :tags '(needs-history)

  (with-tab-history :pre t

    (should (partial-recall--mapped-buffer-p (current-buffer)))))

(ert-deftest pr--should-extend-memory-p ()
  :tags '(needs-history)

  (let ((seconds '(10 11 12))
        (partial-recall-memory-size 1)
        (partial-recall-max-age 2))

    (with-tab-history
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

        (partial-recall--remember (current-buffer))

        (let ((memory (partial-recall--reality)))

          (should (partial-recall--should-extend-memory-p memory))
          (should-not (partial-recall--should-extend-memory-p memory)))))))

(ert-deftest pr--has-buffers-p ()
  :tags '(needs-history)

  (with-tab-history
    (should-not (partial-recall--has-buffers-p))

    (partial-recall--remember (current-buffer))

    (should (partial-recall--has-buffers-p))))

(ert-deftest pr--meaningful-buffer-p ()
  (let ((partial-recall-meaningful-traits '(buffer-file-name)))

    (ert-with-temp-file file
      :buffer buffer
      (with-current-buffer buffer
        (should (partial-recall--meaningful-buffer-p (current-buffer)))

        (let ((partial-recall-filter (list (buffer-name)))
              (partial-recall-meaningful-traits '(partial-recall--not-filtered-p)))

          (should-not (partial-recall--meaningful-buffer-p (current-buffer))))))))

(ert-deftest pr--meaningful-buffer-p--no-traits ()
  (with-temp-buffer
    (let ((partial-recall-meaningful-traits nil))
      (should (partial-recall--meaningful-buffer-p (current-buffer))))))

(ert-deftest pr--meaningful-buffer-p--bad-traits ()
  (with-temp-buffer
    (let ((partial-recall-meaningful-traits '(point)))

      (bydi (partial-recall--warn)
        (should (partial-recall--meaningful-buffer-p (current-buffer)))

        (bydi-was-called-with partial-recall--warn
          (list "Function '%s' has the wrong arity" 'point))))))

(ert-deftest pr--meaningful-buffer-p--calls-traits-with-buffer ()
  (defun pr--trait-test (buffer)
    "Return BUFFER as is."
    buffer)

  (bydi ((:spy pr--trait-test))

    (let ((partial-recall-meaningful-traits '(pr--trait-test)))

      (partial-recall--meaningful-buffer-p (current-buffer))

      (bydi-was-called-with pr--trait-test (current-buffer)))))

;; -- Utility

(ert-deftest pr--warn ()
  (bydi (display-warning)
    (partial-recall--warn "Testing")
    (bydi-was-called-with display-warning (list 'partial-recall "Testing" :warning))))

(ert-deftest pr--log ()
  (let ((partial-recall-log nil)
        (partial-recall-log-level 1)
        (partial-recall-log-prefix nil))

    (should-not (partial-recall--log "test: %s %s" "one" "two"))

    (setq partial-recall-log t)

    (shut-up
      (ert-with-message-capture messages
        (partial-recall--log "test: %s %s" "one" "two")

        (partial-recall--debug "test: %s" "three")

        (setq partial-recall-log-level 0)

        (partial-recall--debug "test: %s" "four")

        (should (string= messages "test: one two\ntest: four\n"))))))

(ert-deftest pr--log--uses-repr ()
  :tags '(needs-history)

  (with-tab-history
    (bydi-with-mock ((:mock format-time-string :return "now")
                     (:mock partial-recall--name :return "test"))

      (let* ((buffer (get-buffer-create "test-repr"))
             (moment (partial-recall--moment-create buffer))
             (partial-recall-log t)
             (partial-recall-log-level 0)
             (partial-recall-log-prefix "Test"))

        (shut-up
          (ert-with-message-capture messages
            (partial-recall--debug "The moment %s was %s" moment "found")
            (should (string= "Test :: The moment #<moment test-repr (now)> was found\n" messages))))

        (kill-buffer buffer)))))

(ert-deftest pr--message--always-calls ()
  (let ((partial-recall-log nil))

    (bydi (partial-recall--log)
      (partial-recall--message "test")

      (bydi-was-called partial-recall--log))))

(ert-deftest pr--repr ()
  (bydi-with-mock ((:mock format-time-string :return "now")
                   (:mock partial-recall--name :return "test"))

    (let* ((partial-recall-memory-size 3)
           (memory (partial-recall--memory-create "test"))
           (buffer (get-buffer-create "test-print"))
           (moment (partial-recall--moment-create buffer)))

      (should (string= "#<memory test (0/3)>" (partial-recall--repr memory)))
      (should (string= "#<moment test-print (now)>" (partial-recall--repr moment)))
      (should (eq 'hello (partial-recall--repr 'hello)))

      (kill-buffer buffer))))

;;; -- Completion

(ert-deftest pr--complete-dream ()
  :tags '(completion)

  (bydi-with-mock (partial-recall--complete-buffer
                   (:always partial-recall--mapped-buffer-p)
                   (:ignore partial-recall--memory-buffer-p))

    (should (funcall (nth 1 (partial-recall--complete-dream "Some prompt: ")) `("buffer" . ,(current-buffer))))))

(ert-deftest pr--complete-dream--initial-input ()
  :tags '(completion)

  (bydi-with-mock (partial-recall--complete-buffer
                   (:ignore partial-recall--memory-buffer-p)
                   (:mock partial-recall--mapped-buffers :return '(third))
                   (:mock current-buffer :return 'third)
                   (:mock buffer-name :with bydi-rf))
    (partial-recall--complete-dream "Some prompt: ")
    (bydi-was-called-with partial-recall--complete-buffer '(... third))))

(ert-deftest pr--complete-reality ()
  :tags '(completion)

  (bydi ((:always partial-recall--memory-buffer-p)
         partial-recall--complete-buffer)

    (should (funcall (nth 1 (partial-recall--complete-reality "Some prompt: ")) `(current . ,(current-buffer))))))

(ert-deftest pr--complete-reality--initial-input ()
  :tags '(completion)

  (bydi (partial-recall--complete-buffer
         (:always partial-recall--memory-buffer-p)
         (:mock partial-recall--mapped-buffers :return '(first second third)))

    (partial-recall--complete-reality "Some prompt: ")

    (bydi-was-called-with partial-recall--complete-buffer (list '... (buffer-name (current-buffer))))))

(ert-deftest pr--complete-subconscious ()
  :tags '(completion)

  (let* ((partial-recall--table (make-hash-table))
         (buf (generate-new-buffer " *temp*" t))
         (sub (partial-recall--subconscious)))

    (ring-insert (partial-recall--memory-ring sub)
                 (partial-recall--moment-create buf))

    (bydi partial-recall--complete-buffer

      (should (funcall
               (nth 1 (partial-recall--complete-subconscious "Some prompt: "))
               `(buf . ,buf))))))

(ert-deftest pr--complete-subconscious--initial-input ()
  :tags '(needs-history completion)

  (with-tab-history :pre t
    (let ((another (generate-new-buffer " *temp*" t)))

      (partial-recall--remember another)
      (partial-recall--forget another t)

      (with-current-buffer another
        (rename-buffer "another-one")

        (bydi (partial-recall--complete-buffer)
          (partial-recall--complete-subconscious "Some prompt: ")

          (bydi-was-called-with partial-recall--complete-buffer `(... "another-one")))))))

(ert-deftest pr--complete-any ()
  :tags '(completion)

  (bydi (partial-recall--complete-buffer
         (:mock partial-recall--meaningful-buffer-p :with (lambda (x) (memq x '(b))))
         (:mock partial-recall--mapped-buffer-p :with (lambda (it &rest _) (memq it '(a c)))))

    (should (funcall
             (nth 1 (partial-recall--complete-any "Some prompt: "))
             '(b . b)))

    (should (funcall
             (nth 1 (partial-recall--complete-any "Some prompt: " t))
             '(a . a)))))

(ert-deftest pr--complete-memory ()
  :tags '(completion)

  (bydi (completing-read
         (:mock partial-recall--memories :return '(a b c))
         (:mock partial-recall--name :with bydi-rf))

    (partial-recall--complete-memory "Some prompt: ")

    (bydi-was-called-with completing-read '(... ((a . a) (b . b) (c . c))))))

(ert-deftest pr--complete-buffer ()
  :tags '(completion)

  (bydi ((:watch minibuffer-completion-table)
         (:spy internal-complete-buffer-except))

    (ert-simulate-keys '(?\C-m)
      (partial-recall--complete-buffer "Some prompt: " #'ignore nil))

    (bydi-was-set minibuffer-completion-table)
    (bydi-was-not-called internal-complete-buffer-except)

    (ert-simulate-keys '(?\C-m)
      (partial-recall--complete-buffer "Some prompt: " #'ignore nil t))

    (bydi-was-set minibuffer-completion-table)
    (bydi-was-called internal-complete-buffer-except)))

;;; -- Lighter

(ert-deftest pr--lighter-title ()
  (should (equal (partial-recall--lighter-title) partial-recall--lighter-title)))

(ert-deftest pr--lighter-moment ()
  :tags '(needs-history)

  (with-tab-history :pre t :wavers t
    (should (equal
             '(:propertize "-"
                           face partial-recall-deemphasized
                           help-echo "Moment is fleeting")
             (partial-recall--lighter-moment)))

    (partial-recall--implant)

    (should (equal '(:propertize "*"
                                 face partial-recall-contrast
                                 help-echo "Moment is implanted")
                   (partial-recall--lighter-moment)))

    (bydi ((:ignore partial-recall--moment-from-buffer)
           (:mock partial-recall--explain-omission :return "testing")
           (:sometimes partial-recall--meaningful-buffer-p))

      (should (equal '(:propertize "?"
                                   face partial-recall-deemphasized
                                   help-echo "Considering")
                     (partial-recall--lighter-moment)))

      (bydi-toggle-sometimes)

      (should (equal '(:propertize "!"
                                   face partial-recall-deemphasized
                                   help-echo  "Not meaningful: testing")
                     (partial-recall--lighter-moment))))))

(ert-deftest partial-recall--lighter-menu ()
  (defvar partial-recall-command-map)

  (let ((partial-recall-command-map (make-sparse-keymap)))

    (defun partial-recall-test ()
      nil)

    (define-key partial-recall-command-map (kbd "t") 'partial-recall-test)

    (bydi (popup-menu)
      (partial-recall--lighter-menu)
      (bydi-was-called popup-menu))))

(ert-deftest partial-recall--lighter-toggle ()
  :tags '(needs-history)

  (with-tab-history :pre t

    (bydi ((:spy partial-recall-implant))

      (partial-recall--lighter-toggle)

      (bydi-was-called-with partial-recall-implant '(... nil))

      (partial-recall--lighter-toggle)

      (bydi-was-called-with partial-recall-implant '(... t)))))

(ert-deftest pr--lighter-memory ()
  :tags '(needs-history)

  (let ((partial-recall-memory-size 1))

    (with-tab-history :pre t :probes t
      (bydi (partial-recall--maybe-reinsert-implanted
             partial-recall--maybe-suppress-oldest-moment)

        (should (equal
                 '(:propertize "."
                               face partial-recall-deemphasized
                               help-echo "Memory contains 1 moment(s)")
                 (partial-recall--lighter-memory))))

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)

        (should (equal
                 '(:propertize "+"
                               face partial-recall-deemphasized
                               help-echo "Memory has grown to +1")
                 (partial-recall--lighter-memory)))))))

;;; -- Setup

(ert-deftest pr--fix-up-primary-tab ()
  :tags '(user-facing)

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

      (bydi-toggle-sometimes)

      (partial-recall--fix-up-primary-tab)
      (bydi-was-called partial-recall--key)
      (bydi-was-called partial-recall--on-create))))

(ert-deftest pr--queue-fix-up ()
  :tags '(user-facing)

  (bydi (run-at-time)
    (partial-recall--queue-fix-up)

    (bydi-was-called-with run-at-time '(... partial-recall--fix-up-primary-tab))))

(ert-deftest pr--setup ()
  (defvar tab-bar-tabs-function nil)
  (defvar tab-bar-mode nil)

  (let ((tab-bar-tabs-function (lambda () (list test-tab)))
        (tab-bar-mode nil))

    (bydi (add-hook
           (:spy advice-add)
           partial-recall--queue-fix-up
           (:mock tab-bar-mode :with (lambda (_) (setq tab-bar-mode t)))
           run-with-timer)

      (partial-recall-mode--setup)

      (bydi-was-called-with run-with-timer '(1 60 partial-recall--concentrate))
      (bydi-was-called partial-recall--queue-fix-up)
      (bydi-was-called-n-times advice-add 3)
      (bydi-was-called-n-times add-hook 7)
      (bydi-was-called tab-bar-mode))))

(ert-deftest pr--teardown ()
  :tags '(user-facing)

  (bydi (remove-hook
         (:spy advice-remove)
         cancel-timer)

    (partial-recall-mode--teardown)

    (bydi-was-called cancel-timer)
    (bydi-was-called-n-times advice-remove 3)
    (bydi-was-called-n-times remove-hook 7)))

;;; -- API

(ert-deftest pr-mode ()
  :tags '(user-facing)

  (bydi (partial-recall-mode--setup
         partial-recall-mode--teardown)

    (partial-recall-mode 1)

    (bydi-was-called partial-recall-mode--setup)

    (partial-recall-mode -1)

    (bydi-was-called partial-recall-mode--teardown)))

(ert-deftest pr--api ()
  :tags '(user-facing)

  (let* ((partial-recall-mode nil)
         (buffer (get-buffer-create "api"))
         (moment (partial-recall--moment-create buffer)))

    (bydi (partial-recall--reinforce
           (:mock partial-recall--reclaim :return moment)
           partial-recall--forget
           partial-recall--complete-dream
           partial-recall--complete-reality
           partial-recall--complete-subconscious
           partial-recall--complete-any
           partial-recall--complete-memory
           partial-recall--implant
           partial-recall--lift
           partial-recall--remember
           partial-recall--switch-to-and-neglect
           partial-recall--meld
           partial-recall--flush
           partial-recall--forget-some
           switch-to-buffer
           (:mock partial-recall--previous-buffer :return 'buffer)
           (:mock partial-recall--next-buffer :return 'buffer))

      (call-interactively 'partial-recall-reclaim)
      (call-interactively 'partial-recall-forget)
      (call-interactively 'partial-recall-switch-to-buffer)
      (funcall-interactively 'partial-recall-switch-to-buffer 'buffer t)
      (call-interactively 'partial-recall-implant)
      (call-interactively 'partial-recall-lift)
      (call-interactively 'partial-recall-remember)
      (call-interactively 'partial-recall-meld)
      (call-interactively 'partial-recall-flush)
      (call-interactively 'partial-recall-next)
      (call-interactively 'partial-recall-previous)
      (call-interactively 'partial-recall-forget-some)

      (bydi-was-called partial-recall--reclaim)
      (bydi-was-called partial-recall--forget)
      (bydi-was-called partial-recall--complete-dream)
      (bydi-was-called partial-recall--complete-reality)
      (bydi-was-called partial-recall--implant)
      (bydi-was-called partial-recall--lift)
      (bydi-was-called partial-recall--remember)
      (bydi-was-called partial-recall--meld)
      (bydi-was-called partial-recall--flush)
      (bydi-was-called partial-recall--next-buffer)
      (bydi-was-called partial-recall--previous-buffer)
      (bydi-was-called partial-recall--forget-some)
      (bydi-was-called-n-times partial-recall--switch-to-and-neglect 5)
      (bydi-was-called-n-times switch-to-buffer 2)
      (bydi-was-called-n-times partial-recall--complete-memory 2))

    (kill-buffer buffer)))

(ert-deftest pr-explain-omission ()
  :tags '(user-facing)

  (let ((explanation nil))
    (shut-up
      (bydi ((:mock partial-recall--explain-omission :return explanation))
        (ert-with-message-capture messages
          (partial-recall-explain-omission)

          (setq explanation "Testing")

          (partial-recall-explain-omission)

          (should (string= messages "Buffer is meaningful or infringed trait has no explanation\nTesting\n")))))))

(ert-deftest pr--is-other-frame-p ()
  (should-not (partial-recall--is-other-frame-p (selected-frame))))

(ert-deftest pr--foreignp ()
  (bydi ((:sometimes partial-recall--tab))

    (should-not (partial-recall--foreignp 'memory))

    (bydi-toggle-sometimes)

    (should (partial-recall--foreignp 'memory))))

;;; partial-recall-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; partial-recall-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

;;; Code:

(require 'partial-recall)
(require 'partial-recall-plasticity)

;;;; Accessors

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
  (with-tab-history (:pre t)

    (let ((memory (partial-recall--reality)))

      (should (eq memory (partial-recall--find-owning-memory))))))

(ert-deftest pr--focus ()
  :tags '(needs-history)
  (let ((partial-recall-memory-size 2))

    (with-tab-history (:pre t)

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (partial-recall--reinforce (current-buffer))

        (should (eq (alist-get 'reinsert partial-recall-intensities) (partial-recall--buffer-focus)))
        (kill-buffer another-temp)))))

(ert-deftest pr--name ()
  :tags '(needs-history)
  (with-tab-history nil
    (let ((reality (partial-recall--reality)))

      (should (string= "test-tab" (partial-recall-memory--name reality))))))

(ert-deftest pr--tab ()
  :tags '(needs-history)
  (with-tab-history (:pre t)
    (should (string= (partial-recall--find-tab-name-from-memory nil (selected-frame))
                     "test-tab"))))

(ert-deftest pr--moment-set-permanence ()
  (ert-with-test-buffer (:name "permanence hook" )
    (let ((moment (partial-recall-moment--create (current-buffer))))

      (add-hook 'partial-recall-permanence-change-hook
                (lambda (m p)
                  (should p)
                  (should (equal moment m)))
                nil t)

      (partial-recall-moment--set-permanence moment t))))

(ert-deftest pr--insert ()
  (ert-with-test-buffer (:name "insert hook")
    (add-hook 'partial-recall-after-insert-hook
              (lambda (it) (should (equal 'item it)))
              nil t)

    (let ((ring (make-ring 1)))

      (partial-recall--ring-insert ring 'item))))

;;;; Handlers

(ert-deftest pr--schedule-buffer--cancels-running-timer ()
  (let ((partial-recall--schedule-timer nil)
        (partial-recall-handle-delay 1)
        (buffer (current-buffer))
        (timer (timer--create)))

    (bydi ((:always buffer-file-name)
           (:ignore partial-recall--buffer-mapped-p)
           partial-recall--void-schedule-timer
           run-at-time)

      (partial-recall--schedule-buffer buffer)

      (bydi-was-called partial-recall--void-schedule-timer)
      (bydi-was-called-with run-at-time `(1 nil partial-recall--handle-buffer ,buffer)))))

(ert-deftest pr--schedule-buffer--ignores-neglected ()
  (with-temp-buffer
    (setq partial-recall--neglect (current-buffer))
    (should-not (partial-recall--schedule-buffer (current-buffer)))
    (should-not partial-recall--neglect)))

(ert-deftest pr--handle-buffer ()
  :tags '(needs-history)
  (with-tab-history nil
    (let ((buffers (list (current-buffer))))

      (bydi (partial-recall--remember
             (:always buffer-live-p)
             (:mock partial-recall--window-list :return buffers)
             (:mock window-buffer :with bydi-rf)
             (:ignore partial-recall--buffer-mapped-p))

        (partial-recall--handle-buffer (current-buffer))
        (bydi-was-called partial-recall--remember)
        (bydi-was-called partial-recall--window-list)))))

(ert-deftest pr--handle-buffer--missing ()
  :tags '(needs-history)
  (with-tab-history nil
    (let ((buffers nil))

      (bydi (partial-recall--remember
             (:always buffer-live-p)
             (:mock window-list :return buffers)
             (:mock window-buffer :with bydi-rf)
             (:ignore partial-recall--buffer-mapped-p))

        (partial-recall--handle-buffer (current-buffer))
        (bydi-was-not-called partial-recall--remember)
        (bydi-was-called window-list)))))

(ert-deftest pr--handle-buffer--reclaims ()
  :tags '(needs-history)
  (with-tab-history nil
    (bydi (partial-recall--remember
           partial-recall--recollect
           (:always buffer-live-p)
           (:mock window-buffer :return (current-buffer))
           (:always partial-recall--buffer-mapped-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--recollect))))

(ert-deftest pr--void-timer--voids ()
  (let* ((timer (timer--create))
         (partial-recall--schedule-timer timer))

    (setf (timer--triggered timer) nil)

    (bydi (cancel-timer)

      (partial-recall--void-schedule-timer)

      (bydi-was-called cancel-timer)

      (should-not partial-recall--schedule-timer))))

(ert-deftest pr--explain-omission-internal ()
  :tags '(meaningful)

  (defun test-meaningful (buffer)
    "Test trait for BUFFER."
    nil)

  (put 'test-meaningful 'partial-recall-non-meaningful-explainer "Testing")

  (bydi ((:othertimes partial-recall--buffer-mapped-p))

    (let ((partial-recall-meaningful-traits '(test-meaningful)))

      (should (string= "Testing" (partial-recall--explain-omission)))

      (bydi-toggle-volatile 'partial-recall--buffer-mapped-p)

      (should (string= "Testing (was remembered explicitly)"
                       (partial-recall--explain-omission))))))

;;;; Reactions

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
  (bydi ((:mock partial-recall--create-key :return "test")
         (:spy run-hook-with-args))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab)))

      (bydi-was-called-with run-hook-with-args '(partial-recall-after-create-hook "test-tab")))))

(ert-deftest pr--on-close ()
  :tags '(needs-history)
  (bydi (partial-recall--suppress)
    (with-tab-history (:pre t)

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

(ert-deftest pr--after-register-val-jump-to ()
  (bydi (partial-recall--maybe-switch-memory)

    (should-not (partial-recall--after-register-val-jump-to nil))
    (bydi-was-not-called partial-recall--maybe-switch-memory)

    (let ((conf (current-window-configuration)))

      (should (partial-recall--after-register-val-jump-to (list conf (point-marker))))
      (bydi-was-called partial-recall--maybe-switch-memory))))

(ert-deftest pr--after-winner ()
  (bydi ((:ignore partial-recall--buffer-in-memory-p)
         partial-recall--maybe-switch-memory)

    (partial-recall--after-winner)

    (bydi-was-called partial-recall--maybe-switch-memory)))

(ert-deftest pr--after-tab-bar-switch ()
  :tags '(hooks)

  (bydi ((:spy run-hook-with-args))

    (partial-recall--after-tab-bar-switch "test")

    (bydi-was-called-with run-hook-with-args '(partial-recall-after-reality-change-hook "test"))))

(ert-deftest pr--before-undo-close-tab ()
  :tags '(advice)

  (let ((tab-bar-closed-tabs '((frame (tab (pr . "test"))))))

    (bydi ((:always frame-live-p))

      (should (string= (partial-recall--before-undo-close-tab)
                       "test")))))

(ert-deftest pr--after-undo-close-tab ()
  :tags '(advice needs-history)

  (with-tab-history (:pre t)
    (let ((another (generate-new-buffer " *temp*" t)))

      (partial-recall--remember another)

      (partial-recall--forget (current-buffer))
      (partial-recall--forget another)

      (let ((partial-recall--restored-tab "test-hash"))

        (partial-recall--after-undo-close-tab)

        (should (eq (length (partial-recall-moments))
                    2))))))

(ert-deftest pr--before-view-buffer ()
  :tags '(reaction)

  (bydi ((:watch partial-recall--to-be-viewed))

    (partial-recall--before-view-buffer (current-buffer))

    (bydi-was-set-to partial-recall--to-be-viewed (current-buffer))))

;;;; Actions

(ert-deftest pr-remember--remembers ()
  :tags '(needs-history)

  (with-tab-history nil

    (partial-recall--remember (current-buffer))

    (should-not (null (gethash (alist-get 'pr test-tab) partial-recall--table)))))

(ert-deftest pr-remember--inserts-once ()
  :tags '(needs-history)

  (defvar partial-recall--table)

  (with-tab-history (:pre t)

    (partial-recall--remember (current-buffer))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall-memory--moments memory)))

      (should (eq 1 (ring-length ring))))))

(ert-deftest pr-remember--extends-ring ()
  :tags '(plasticity needs-history)

  (with-memory-plasticity-enabled
    (with-tab-history (:probes t)
      (bydi ((:always partial-recall-memory--near-capacity-p)
             (:always partial-recall-plasticity--should-extend-p)
             partial-recall--maybe-reinserted-implanted
             partial-recall--maybe-suppress-oldest-moment
             partial-recall--maybe-forget-oldest-moment
             ring-extend)

        (partial-recall--remember (current-buffer))
        (bydi-was-called ring-extend)))))

(ert-deftest pr-remember--removes-impermanent ()
  :tags '(needs-history)

  (let ((partial-recall-memory-size 1))
    (with-tab-history (:pre t :probes t)
      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (bydi ((:always partial-recall-memory--near-capacity-p)
               partial-recall--maybe-reinsert-implanted
               partial-recall--suppress)
          (partial-recall--remember another-temp)

          (bydi-was-called partial-recall--suppress))

        (kill-buffer another-temp)))))

(ert-deftest pr-swap ()
  :tags '(plasticity)

  (with-memory-plasticity-enabled
    (let* ((partial-recall-memory-size 1)
           (partial-recall-intensities '((swap . -1)))
           (a (partial-recall-memory--create "a"))
           (b (partial-recall-memory--create "b"))
           (buffer (generate-new-buffer " *temp*" t))
           (moment (partial-recall-moment--create buffer)))

      (ring-insert (partial-recall-memory--moments a) moment)

      (setf (partial-recall-moment--focus moment) 10)
      (setf (partial-recall-moment--permanence moment) t)

      (bydi ((:mock partial-recall--find-tab-name-from-memory :return "tab")
             (:mock buffer-name :return "buffer")
             partial-recall--maybe-reinsert-implanted
             partial-recall-moment--update-timestamp
             (:always partial-recall-memory--near-capacity-p)
             (:always partial-recall-plasticity--should-extend-p))

        (partial-recall--swap a b moment)

        (bydi-was-called partial-recall-moment--update-timestamp)

        ;; Check that swapping can also reduce focus of permanent moments.
        (should (eq 9 (partial-recall-moment--focus moment)))

        (let ((ring (partial-recall-memory--moments b)))

          (should (eq 2 (ring-size ring)))
          (should (ring-member ring moment)))))))

(ert-deftest pr-recollect--reinforces-reality-or-reclaims ()
  (bydi ((:always partial-recall--buffer-mapped-p)
         (:sometimes partial-recall--buffer-in-memory-p)
         partial-recall--reinforce
         partial-recall--reclaim
         (:othertimes partial-recall--short-term-p))

    (partial-recall--recollect (current-buffer))

    (bydi-was-called partial-recall--reinforce)

    (bydi-toggle-sometimes)

    (partial-recall--recollect (current-buffer))
    (bydi-was-called partial-recall--reclaim)))

(ert-deftest pr-recollect--signals-error ()
  (bydi ((:ignore partial-recall--buffer-mapped-p))

    (should-error (partial-recall--recollect (current-buffer)))))

(ert-deftest pr-reinforce--reinforces-old-buffers ()
  :tags '(needs-history)

  (with-tab-history nil
    (let ((count nil)
          (seconds '(6 8 10 12))
          (get-count (lambda ()
                       (let* ((reality (partial-recall--reality))
                              (moments (partial-recall-memory--moments reality))
                              (buffer (current-buffer))
                              (moment (ring-ref moments (partial-recall-memory--owns-buffer-p reality buffer))))
                         (partial-recall-moment--focus moment))))
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
        (partial-recall-intermediate-term -1)
        (mock-reality (partial-recall-memory--create "other-key")))

    (with-tab-history nil
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

        (partial-recall--remember (current-buffer))

        (bydi ((:mock partial-recall--reality :return mock-reality)
               partial-recall--swap)

          (let ((moment (partial-recall--find-owning-moment (current-buffer))))

            (partial-recall-moment--set-permanence moment t)

            (partial-recall--reclaim (current-buffer))

            (bydi-was-not-called partial-recall--swap)

            (partial-recall-moment--set-permanence moment nil)

            (partial-recall--reclaim (current-buffer))

            (bydi-was-called partial-recall--swap)))))))

(ert-deftest pr-reclaim--no-op-for-same ()
  :tags '(needs-history)

  (with-tab-history (:pre t)

    (bydi (partial-recall--remember)

      (partial-recall--reclaim (current-buffer))

      (bydi-was-not-called partial-recall--remember))))

(ert-deftest pr-forget--forgets ()
  :tags '(needs-history history)

  (with-tab-history (:pre t)

    (bydi (partial-recall--suppress
           partial-recall--banish)
      (partial-recall--forget (current-buffer) t)

      (bydi-was-called partial-recall--suppress)
      (bydi-was-called partial-recall--banish))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall-memory--moments memory)))

      (should (eq 0 (ring-length ring))))))

(ert-deftest pr--forget-some ()
  :tags '(needs-history)

  (with-tab-history (:pre t)

    (bydi ((:always yes-or-no-p)
           partial-recall--forget
           (:mock partial-recall-repr :return "#<test-moment>"))

      (partial-recall--forget-some)

      (bydi-was-called-with yes-or-no-p "Forget `#<test-moment>' (unmodified)?")

      (bydi-was-called-with partial-recall--forget (list (current-buffer))))))

(ert-deftest pr--remember-some ()
  :tags '(needs-history)

  (with-tab-history (:pre t)

    (bydi ((:sometimes yes-or-no-p)
           (:spy partial-recall--remember)
           (:watch partial-recall--disturbed))

      (rename-buffer "some-remember")

      (partial-recall--forget)

      (partial-recall--remember-some)

      (bydi-was-called-with yes-or-no-p "Remember lost buffer `some-remember'?")
      (bydi-was-called-with partial-recall--remember (current-buffer) :clear t)

      (partial-recall--forget)
      (bydi-toggle-volatile 'yes-or-no-p)
      (partial-recall--remember-some)

      (bydi-was-not-called partial-recall--remember)
      (bydi-was-set partial-recall--disturbed))))

(ert-deftest pr-forget--shortens-extended-memory ()
  :tags '(plasticity needs-history)

  (let ((partial-recall-memory-size 2)
        (another-temp (generate-new-buffer " *temp*" t))
        (yet-another-temp (generate-new-buffer " *temp*" t)))

    (bydi (partial-recall--suppress
           partial-recall-plasticity--maybe-reinsert-implanted
           (:sometimes partial-recall--short-term-p))
      (with-moment-plasticity-enabled
        (with-memory-plasticity-enabled
          (with-tab-history (:pre t :probes t)
            (let ((ring (partial-recall-memory--moments (partial-recall--reality))))

              (partial-recall--remember another-temp)
              (partial-recall--remember yet-another-temp)

              (should (eq (ring-size ring) 3))
              (should (eq (ring-length ring) 3))

              (bydi-toggle-sometimes)

              (partial-recall--flush (partial-recall--reality))

              (should (eq (ring-size ring) 2))
              (should (eq (ring-length ring) 0))

              (kill-buffer another-temp)
              (kill-buffer yet-another-temp))))))))

(ert-deftest pr--banish ()
  (ert-with-test-buffer (:name "banished")

    (partial-recall--banish (current-buffer))

    (should partial-recall--banished)))

(ert-deftest partial-recall--reject ()
  (with-tab-history (:pre t :second-mem t)
    (should-error (partial-recall--reject (current-buffer) (partial-recall--reality)))

    (let* ((another (generate-new-buffer " *temp*" t))
           (moment (partial-recall-moment--create another)))

      (ring-insert (partial-recall-memory--moments second-memory) moment)

      (should-error (partial-recall--reject another second-memory))

      (kill-buffer another))

    (bydi (partial-recall--swap
           partial-recall--clean-up-buffer)

      (partial-recall-reject (current-buffer) second-memory)

      (bydi-was-called partial-recall--swap)
      (bydi-was-called partial-recall--clean-up-buffer))))

(ert-deftest partial-recall--suppress--just-removes ()
  :tags '(needs-history)

  (let ((partial-recall-repress nil))

    (with-tab-history nil

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (partial-recall--remember (current-buffer))
        (partial-recall--forget another-temp)

        (bydi ((:always partial-recall-memory--near-capacity-p)
               kill-buffer)
          (partial-recall--forget)

          (bydi-was-not-called kill-buffer))

        (kill-buffer another-temp)))))

(ert-deftest pr--suppressed-p ()
  :tags '(subconscious)

  (bydi ((:mock partial-recall--suppressed :return '(a b c)))

    (should (partial-recall--suppressed-p 'b))
    (should-not (partial-recall--suppressed-p 'd))))

(ert-deftest pr--suppressed ()
  :tags '(subconscious)

  (ert-with-test-buffer (:name "suppressed")
    (setq-local partial-recall--remnant "best")
    (bydi ((:mock partial-recall--key :return "test")
           (:mock buffer-list :return (list (current-buffer))))

      (should-not (partial-recall--suppressed))

      (should (partial-recall--suppressed "best")))))

(ert-deftest pr--maybe-switch-memory ()
  :tags '(needs-history)

  (let ((partial-recall-auto-switch 'prompt)
        (user-input nil))

    (with-tab-history (:pre t :second-mem t)
      (let* ((another (generate-new-buffer " *temp*" t))
             (moment (partial-recall-moment--create another)))

        (ring-insert (partial-recall-memory--moments second-memory) moment)

        (should-not (partial-recall--maybe-switch-memory))

        (bydi (tab-bar-switch-to-tab
               (:mock partial-recall--find-tab-name-from-memory :return "test")
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
  (let ((partial-recall--last-handled (current-buffer)))

    (partial-recall--clean-up-buffer (current-buffer))

    (should-not partial-recall--last-handled))

  (bydi ((:mock window-buffer :return (current-buffer))
         quit-window)

    (partial-recall--clean-up-buffer (current-buffer))

    (bydi-was-called quit-window)))

(ert-deftest pr--clean-up-buffer--clears-focus ()
  :tags '(needs-history)

  (with-tab-history (:pre t)
    (bydi ((:mock window-buffer :return (current-buffer))
           quit-window)

      (should partial-recall--last-focus)

      (partial-recall--clean-up-buffer (current-buffer))

      (should-not partial-recall--last-focus))))

(ert-deftest pr--meld ()
  :tags '(needs-history)

  (with-tab-history (:pre t :second-mem t)
    (should-error (partial-recall--meld (partial-recall--reality) (partial-recall--reality)))

    (bydi (tab-bar-close-tab-by-name)
      (partial-recall--meld (partial-recall--reality) second-memory t)

      (should (ring-empty-p (partial-recall-memory--moments (partial-recall--reality))))

      (should (partial-recall--buffer-in-memory-p (current-buffer) second-memory))

      (bydi-was-called-with tab-bar-close-tab-by-name "test-tab"))))

(ert-deftest pr--spin-out ()

  (bydi ((:mock read-string :return "Test")
         tab-bar-new-tab
         tab-bar-rename-tab
         switch-to-buffer
         partial-recall--reclaim)

    (partial-recall--spin-out '(a b c))

    (bydi-was-called tab-bar-new-tab)
    (bydi-was-called-with tab-bar-rename-tab "Test")

    (bydi-was-called-with switch-to-buffer (list 'a t))

    (bydi-was-called-n-times partial-recall--reclaim 3)))


(ert-deftest pr--flush ()
  :tags '(needs-history plasticity)

  (with-moment-plasticity-enabled
    (with-tab-history (:pre t)
      (let ((another (generate-new-buffer " *temp*" t))
            (ring (partial-recall-memory--moments (partial-recall--reality)))
            (partial-recall-intermediate-term -1))

        (partial-recall--remember another)

        (should (eq 2 (ring-length ring)))

        (partial-recall--set-permanence another)

        (bydi (partial-recall--clean-up-buffer)
          (partial-recall--flush (partial-recall--reality))

          (bydi-was-called partial-recall--clean-up-buffer))

        (bydi (partial-recall--clean-up-buffer
               (:always partial-recall--buffer-visible-p))
          (partial-recall--flush (partial-recall--reality) nil t)

          (bydi-was-not-called partial-recall--clean-up-buffer))

        (should (eq 1 (ring-length ring)))
        (should (eq another (partial-recall-moment--buffer (ring-ref ring 0))))

        (with-current-buffer another
          (partial-recall--set-permanence another t)

          (partial-recall--flush (partial-recall--reality) t)

          (should (eq 1 (ring-length ring)))
          (should (eq another (partial-recall-moment--buffer (ring-ref ring 0)))))))))

(ert-deftest pr--switch-to-and-neglect ()
  (let ((partial-recall--switch-to-buffer-function 'switch-to-buffer))

    (bydi (switch-to-buffer)

      (with-temp-buffer
        (partial-recall--switch-to-buffer-and-neglect (current-buffer))

        (should (eq (current-buffer) partial-recall--neglect))

        (bydi-was-called-with switch-to-buffer (current-buffer))))))

(ert-deftest pr--next-and-prev ()
  :tags '(needs-history)

  (with-tab-history (:pre t)
    (let ((another (generate-new-buffer " *temp*" t)))

      (partial-recall--remember another)

      (should (equal (partial-recall--next-buffer) another))
      (should (equal (partial-recall--previous-buffer) another)))))

(ert-deftest partial-recall--probe-memory--runs-hooks ()
  :tags '(plasticity)

  (bydi (partial-recall--maybe-reinsert-implanted
         partial-recall--maybe-suppress-oldest-moment)

    (ert-with-test-buffer (:name "memory hook")
      (add-hook
       'partial-recall-after-probe-hook
       (lambda (memory)
         (should (equal 'memory memory)))
       nil t)

      (partial-recall--probe-memory 'memory))))

;;;; Conditionals

(ert-deftest pr--memory-buffer-p ()
  :tags '(needs-history)

  (with-tab-history (:pre t)

    (let ((memory (gethash (alist-get 'pr test-tab) partial-recall--table)))

      (should (partial-recall--buffer-in-memory-p (current-buffer) memory)))))

(ert-deftest pr-moment-buffer-p ()
  :tags '(needs-history)

  (with-tab-history (:pre t)

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall-memory--moments memory))
           (moment (ring-ref ring 0)))

      (should (partial-recall--buffer-equals-moment-p (current-buffer) moment)))))

(ert-deftest pr--mapped-buffer-p ()
  :tags '(needs-history)

  (with-tab-history (:pre t)

    (should (partial-recall--buffer-mapped-p (current-buffer)))))

(ert-deftest pr--has-buffers-p ()
  :tags '(needs-history)

  (with-tab-history nil
    (should-not (partial-recall-memory--has-buffers-p))

    (partial-recall--remember (current-buffer))

    (should (partial-recall-memory--has-buffers-p))))

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

      (bydi (partial-recall-warn)
        (should (partial-recall--meaningful-buffer-p (current-buffer)))

        (bydi-was-called-with partial-recall-warn
          (list "Function `%s' has the wrong arity" 'point))))))

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
    (partial-recall-warn "Testing")
    (bydi-was-called-with display-warning (list 'partial-recall "Testing" :warning))))

(defun kill-log-buffer ()
  "Kill the log buffer if it exists."
  (when-let ((buffer (get-buffer partial-recall-log--buffer-name)))
    (kill-buffer buffer)))

(ert-deftest pr--log ()
  (let ((partial-recall-log nil)
        (partial-recall-log-prefix nil)
        (partial-recall-log-echo t))

    (should-not (partial-recall-log "test: %s %s" "one" "two"))

    (setq partial-recall-log 1)

    (shut-up
      (bydi (partial-recall-log--write-to-buffer
             (:watch message-log-max))
        (ert-with-message-capture messages
          (partial-recall-log "test: %s %s" "one" "two")

          (bydi-was-set message-log-max)

          (partial-recall-debug "test: %s" "three")

          (setq partial-recall-log 0)

          (partial-recall-debug "test: %s" "four")

          (should (string= messages "test: one two\ntest: four\n")))))

    (kill-log-buffer)))

(ert-deftest pr--log--uses-repr ()
  :tags '(needs-history)

  (with-tab-history nil
    (bydi-with-mock ((:mock format-time-string :return "now")
                     (:mock partial-recall-memory--name :return "test")
                     partial-recall-log--write-to-buffer)

      (let* ((buffer (get-buffer-create "test-repr"))
             (moment (partial-recall-moment--create buffer))
             (partial-recall-log 0)
             (partial-recall-log-prefix "Test")
             (partial-recall-log-echo t))

        (shut-up
          (ert-with-message-capture messages
            (partial-recall-debug "The moment %s was %s" moment "found")
            (should (string= "Test :: The moment #<moment test-repr (now)> was found\n" messages)))))))

  (kill-log-buffer))

(ert-deftest pr--log--writes-to-buffer ()
  (bydi ((:mock format-time-string :return "[time] "))
    (let ((partial-recall-log nil))

      (partial-recall-log "This is a %s" "test")

      (should-not (get-buffer partial-recall-log--buffer-name))

      (setq partial-recall-log 0)

      (shut-up
        (partial-recall-log "This is the %s message" "first")
        (partial-recall-debug "This %s the %s message" "will be" "second"))

      (with-current-buffer (get-buffer partial-recall-log--buffer-name)
        (should (string= (buffer-string)
                         "[time] This is the first message\n[time] This will be the second message\n")))))

  (kill-log-buffer))

(ert-deftest pr--repr ()
  (bydi-with-mock ((:mock format-time-string :return "now")
                   (:mock partial-recall-memory--name :return "test"))

    (let* ((partial-recall-memory-size 3)
           (memory (partial-recall-memory--create "test"))
           (buffer (get-buffer-create "test-print"))
           (moment (partial-recall-moment--create buffer)))

      (should (string= "#<memory test (0/3)>" (partial-recall-repr memory)))
      (should (string= "#<moment test-print (now)>" (partial-recall-repr moment)))
      (should (eq 'hello (partial-recall-repr 'hello)))

      (kill-buffer buffer))

    (kill-log-buffer)))

;;;; Completion

(ert-deftest pr--complete-dream ()
  :tags '(completion)

  (bydi-with-mock (partial-recall--complete-buffer
                   (:always partial-recall--buffer-mapped-p)
                   (:ignore partial-recall--buffer-in-memory-p))

    (should (funcall (nth 1 (partial-recall--complete-dream "Some prompt: ")) `("buffer" . ,(current-buffer))))))

(ert-deftest pr--complete-dream--initial-input ()
  :tags '(completion)

  (bydi-with-mock (partial-recall--complete-buffer
                   (:ignore partial-recall--buffer-in-memory-p)
                   (:mock partial-recall-buffers :return '(third))
                   (:mock current-buffer :return 'third)
                   (:mock buffer-name :with bydi-rf))
    (partial-recall--complete-dream "Some prompt: ")
    (bydi-was-called-with partial-recall--complete-buffer '(... third))))

(ert-deftest pr--complete-reality ()
  :tags '(completion)

  (bydi ((:always partial-recall--buffer-in-memory-p)
         partial-recall--complete-buffer)

    (should (funcall (nth 1 (partial-recall--complete-reality "Some prompt: ")) `(current . ,(current-buffer))))))

(ert-deftest pr--complete-reality--initial-input ()
  :tags '(completion)

  (bydi (partial-recall--complete-buffer
         (:always partial-recall--buffer-in-memory-p)
         (:mock partial-recall-buffers :return '(first second third)))

    (partial-recall--complete-reality "Some prompt: ")

    (bydi-was-called-with partial-recall--complete-buffer (list '... (buffer-name (current-buffer))))))

(ert-deftest pr--complete-subconscious ()
  :tags '(completion)

  (bydi (partial-recall--complete-buffer
         (:always partial-recall--suppressed-p))

    (should (funcall (nth 1 (partial-recall--complete-subconscious "Some prompt"))
                     `(current . ,(current-buffer))))))

(ert-deftest pr--complete-subconscious--initial-input ()
  :tags '(completion)

  (bydi (partial-recall--complete-buffer
         (:always partial-recall--suppressed-p))

    (partial-recall--complete-subconscious "Some prompt")

    (bydi-was-called-with partial-recall--complete-buffer (list '... (buffer-name (current-buffer))))))

(ert-deftest pr--complete-removed ()
  :tags '(completion)

  (bydi (partial-recall--complete-buffer
         (:mock partial-recall-memory--removed-buffers :return (list (current-buffer))))

    (should (funcall (nth 1 (partial-recall--complete-removed "Some prompt: "))
                     `(current . ,(current-buffer))))))


(ert-deftest pr--complete-any ()
  :tags '(completion)

  (bydi (partial-recall--complete-buffer
         (:mock partial-recall--meaningful-buffer-p :with (lambda (x) (memq x '(b))))
         (:mock partial-recall--buffer-mapped-p :with (lambda (it &rest _) (memq it '(a c)))))

    (should (funcall
             (nth 1 (partial-recall--complete-any "Some prompt: "))
             '(b . b)))

    (should (funcall
             (nth 1 (partial-recall--complete-any "Some prompt: " t))
             '(a . a)))))

(ert-deftest pr--complete-memory ()
  :tags '(completion)

  (bydi (completing-read
         (:mock partial-recall-memories :return '(a b c))
         (:mock partial-recall-memory--name :with bydi-rf))

    (partial-recall--complete-memory "Some prompt: ")

    (bydi-was-called-with completing-read '(... ((a . a) (b . b) (c . c))))))

(ert-deftest pr--complete-buffer ()
  :tags '(completion)

  (bydi ((:watch minibuffer-completion-table)
         (:spy internal-complete-buffer-except))

    (ert-simulate-keys '(?\C-m)
      (should-error (partial-recall--complete-buffer "Some prompt: " #'ignore nil)))

    (bydi-was-set minibuffer-completion-table)
    (bydi-was-not-called internal-complete-buffer-except)

    (ert-simulate-keys '(?\C-m)
      (should-error (partial-recall--complete-buffer "Some prompt: " #'ignore nil t)))

    (bydi-was-set minibuffer-completion-table)
    (bydi-was-called internal-complete-buffer-except)

    (ert-simulate-keys '(?\C-m)
      (should (equal
               (partial-recall--complete-buffer "Some prompt: " #'always (buffer-name (current-buffer)))
               (current-buffer))))))

;;;; Lighter

(ert-deftest pr-lighter-moment ()
  :tags '(needs-history plasticity)

  (with-tab-history (:pre t :wavers t)
    (should (equal
             '(:propertize "-"
                           face partial-recall-deemphasized
                           help-echo "Moment is fleeting")
             (partial-recall-lighter--moment)))

    (with-moment-plasticity-enabled
      (let ((partial-recall-plasticity-implant-threshold 20)
            (partial-recall-intensities '((test . 1))))

        (partial-recall-moment--adjust (partial-recall-current-moment) 'test)

        (should (equal
                 '(:propertize "▁"
                               face partial-recall-deemphasized
                               help-echo "Moment is fleeting (focus 5%)")
                 (partial-recall-lighter--moment))))

      (partial-recall--set-permanence)

      (should (equal '(:propertize "*"
                                   face partial-recall-contrast
                                   help-echo "Moment is implanted")
                     (partial-recall-lighter--moment))))

    (let ((explanation "testing"))

      (bydi ((:ignore partial-recall--find-owning-moment)
             (:mock partial-recall--explain-omission :return explanation)
             (:sometimes partial-recall--meaningful-buffer-p))

        (should (equal '(:propertize "?"
                                     face partial-recall-deemphasized
                                     help-echo "Considering")
                       (partial-recall-lighter--moment)))

        (bydi-toggle-sometimes)

        (should (equal '(:propertize "!"
                                     face partial-recall-emphasis
                                     help-echo  "Not meaningful: testing")
                       (partial-recall-lighter--moment)))

        (setq explanation nil)

        (should (equal '(:propertize "!"
                                     face partial-recall-deemphasized
                                     help-echo  "Not meaningful")
                       (partial-recall-lighter--moment)))))))

(ert-deftest partial-recall-lighter--moment--foreign ()
  (bydi ((:always partial-recall--find-owning-moment)
         (:ignore partial-recall-moment--focus-percentage)
         (:ignore partial-recall--buffer-in-memory-p))

    (should (equal '(:propertize "!"
                                 face partial-recall-contrast
                                 help-echo "Moment is foreign")
                   (partial-recall-lighter--moment)))))

(ert-deftest partial-recall-lighter--menu ()
  (defvar partial-recall-command-map)

  (let ((partial-recall-command-map (make-sparse-keymap)))

    (defun partial-recall-test ()
      nil)

    (define-key partial-recall-command-map (kbd "t") 'partial-recall-test)

    (bydi (popup-menu)
      (partial-recall-lighter--menu)
      (bydi-was-called popup-menu))))

(ert-deftest partial-recall-lighter--toggle ()
  :tags '(needs-history)

  (with-tab-history (:pre t)

    (bydi ((:spy partial-recall-make-permanent))

      (partial-recall-lighter--toggle)

      (bydi-was-called-with partial-recall-make-permanent '(... nil))

      (partial-recall-lighter--toggle)

      (bydi-was-called-with partial-recall-make-permanent '(... t)))))

(ert-deftest pr-lighter-memory ()
  :tags '(plasticity needs-history)

  (let ((partial-recall-memory-size 1))

    (with-tab-history (:pre t :probes t)
      (bydi (partial-recall--maybe-reinsert-implanted
             partial-recall--maybe-suppress-oldest-moment
             (:mock partial-recall-graph :return "|"))

        (should (equal
                 '(:propertize "|"
                               face partial-recall-contrast
                               help-echo "Memory contains 1/1 moment(s)")
                 (partial-recall-lighter--memory))))

      (with-memory-plasticity-enabled
        (let ((another-temp (generate-new-buffer " *temp*" t)))

          (partial-recall--remember another-temp)

          (should (equal
                   '(:propertize "+"
                                 face partial-recall-emphasis
                                 help-echo "Memory has grown to +1")
                   (partial-recall-lighter--memory))))))))

;;;; Setup

(ert-deftest pr--fix-up-primary-tab ()
  :tags '(user-facing)

  (let ((tab-bar-mode nil)
        (tab-bar-tabs-function (lambda () (list test-tab))))

    (bydi (partial-recall--on-create
           (:sometimes partial-recall--key)
           partial-recall-warn)

      (partial-recall--fix-up-primary-tab)

      (bydi-was-called partial-recall-warn)
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
    (partial-recall--queue-tab-fix-up)

    (bydi-was-called-with run-at-time '(... partial-recall--fix-up-primary-tab))))

(ert-deftest pr--setup ()
  (defvar tab-bar-tabs-function nil)
  (defvar tab-bar-mode nil)

  (let ((tab-bar-tabs-function (lambda () (list test-tab)))
        (tab-bar-mode nil))

    (bydi (add-hook
           (:spy advice-add)
           partial-recall--queue-tab-fix-up
           (:mock tab-bar-mode :with (lambda (_) (setq tab-bar-mode t)))
           run-with-timer)

      (partial-recall-mode--setup)

      (bydi-was-called partial-recall--queue-tab-fix-up)
      (bydi-was-called-n-times advice-add 10)
      (bydi-was-called-n-times add-hook 7)
      (bydi-was-called tab-bar-mode))))

(ert-deftest pr--teardown ()
  :tags '(user-facing)

  (bydi (remove-hook
         (:spy advice-remove))

    (partial-recall-mode--teardown)

    (bydi-was-called-n-times advice-remove 10)
    (bydi-was-called-n-times remove-hook 7)))

;;;; API

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
         (moment (partial-recall-moment--create buffer)))

    (bydi (partial-recall--reinforce
           (:mock partial-recall--reclaim :return moment)
           partial-recall--forget
           partial-recall--complete-dream
           partial-recall--complete-reality
           partial-recall--complete-subconscious
           partial-recall--complete-any
           partial-recall--complete-memory
           partial-recall--complete-removed
           partial-recall--set-permanence

           partial-recall--remember
           partial-recall--remember-some
           partial-recall--switch-to-buffer-and-neglect
           partial-recall--meld
           partial-recall--flush
           partial-recall--forget-some
           partial-recall--reject
           switch-to-buffer
           pop-to-buffer
           (:mock partial-recall--previous-buffer :return 'buffer)
           (:mock partial-recall--next-buffer :return 'buffer))

      (call-interactively 'partial-recall-reclaim)
      (call-interactively 'partial-recall-forget)
      (call-interactively 'partial-recall-switch-to-buffer)
      (call-interactively 'partial-recall-switch-to-buffer-other-window)
      (funcall-interactively 'partial-recall-switch-to-buffer 'buffer t)
      (call-interactively 'partial-recall-make-permanent)
      (call-interactively 'partial-recall-lift)
      (call-interactively 'partial-recall-remember)
      (call-interactively 'partial-recall-meld)
      (call-interactively 'partial-recall-flush)
      (call-interactively 'partial-recall-next)
      (call-interactively 'partial-recall-previous)
      (call-interactively 'partial-recall-forget-some)
      (call-interactively 'partial-recall-reject)
      (call-interactively 'partial-recall-retrieve)
      (call-interactively 'partial-recall-banish)
      (call-interactively 'partial-recall-remember-some)

      (bydi-was-called partial-recall--reclaim)
      (bydi-was-called partial-recall--forget)
      (bydi-was-called partial-recall--set-permanence)
      (bydi-was-called partial-recall--remember)
      (bydi-was-called partial-recall--meld)
      (bydi-was-called partial-recall--flush)
      (bydi-was-called partial-recall--next-buffer)
      (bydi-was-called partial-recall--previous-buffer)
      (bydi-was-called partial-recall--forget-some)
      (bydi-was-called partial-recall--remember-some)
      (bydi-was-called partial-recall--reject)
      (bydi-was-called partial-recall--complete-removed)

      (bydi-was-called-n-times partial-recall--complete-reality 6)
      (bydi-was-called-n-times partial-recall--switch-to-buffer-and-neglect 6)
      (bydi-was-called-n-times switch-to-buffer 2)
      (bydi-was-called-n-times pop-to-buffer 1)
      (bydi-was-called-n-times partial-recall--complete-dream 1)
      (bydi-was-called-n-times partial-recall--complete-memory 3))

    (kill-buffer buffer)))

(ert-deftest pr-pop-to-logs ()
  (let ((partial-recall-log 0))
    (bydi (view-buffer-other-window)
      (shut-up
        (partial-recall-log "Make sure it exists"))

      (partial-recall-pop-to-logs)
      (bydi-was-called view-buffer-other-window)

      (kill-buffer partial-recall-log--buffer-name)

      (should-error (partial-recall-pop-to-logs)))))

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
  (bydi ((:sometimes partial-recall--find-tab-from-memory))

    (should-not (partial-recall--foreignp 'memory))

    (bydi-toggle-sometimes)

    (should (partial-recall--foreignp 'memory))))

(ert-deftest pr-graph--nothing-for-zeroes ()
  (should-not (partial-recall-graph 0 1))
  (should-not (partial-recall-graph 1 0))
  (should-not (partial-recall-graph 2 nil)))


;;;; History

(ert-deftest memory--removed-buffers ()
  :tags '(history needs-history)

  (with-tab-history (:pre t)

    (partial-recall--forget)

    (should (eq (length (partial-recall-memory--removed-buffers)) 1))))

;;; partial-recall-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

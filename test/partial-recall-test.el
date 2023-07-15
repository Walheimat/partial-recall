;;; partial-recall-test.el --- Tests for package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `partial-recall'.

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
  (with-tab-history :pre t

    (let ((memory (partial-recall--reality)))

      (should (eq memory (partial-recall--buffer-owner))))))

(ert-deftest pr--update-count ()
  (let ((partial-recall-buffer-limit 2))

    (with-tab-history :pre t

      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--remember another-temp)
        (partial-recall--reinforce (current-buffer))

        (should (eq 1 (partial-recall--update-count)))
        (kill-buffer another-temp)))))

(ert-deftest pr--name ()
  (with-tab-history
    (let ((reality (partial-recall--reality)))

      (bydi ((:sometimes partial-recall--subconscious-p))

        (should (string= partial-recall--subconscious-key (partial-recall--name reality)))

        (bydi-toggle-sometimes)

        (should (string= "test-tab" (partial-recall--name reality)))))))

(ert-deftest pr--tab ()
  (with-tab-history :pre t
    (should (string= (partial-recall--tab-name) "test-tab"))))

(ert-deftest pr--ensure-subconscious ()
  (let ((partial-recall-table (make-hash-table)))

    (partial-recall--subconscious)

    (should (gethash partial-recall--subconscious-key partial-recall--table))))

(ert-deftest partial-recall--lifted ()
  (with-tab-history :pre t :lifts t
    (partial-recall--forget (current-buffer) t)

    (should (partial-recall--lifted (current-buffer)))))

;;; -- Handlers

(ert-deftest pr--after-switch-to-buffer--cancels-running-timer ()
  (let ((partial-recall--timer nil)
        (partial-recall-handle-delay 1)
        (buffer (current-buffer)))

    (bydi ((:always buffer-file-name)
           (:ignore partial-recall--mapped-buffer-p)
           cancel-timer
           run-at-time)

      (setq partial-recall--timer 'timer)
      (partial-recall--after-switch-to-buffer buffer)

      (bydi-was-called cancel-timer)

      (bydi-was-called-with run-at-time `(1 nil partial-recall--handle-buffer ,buffer)))))

(ert-deftest pr--handle-buffer ()
  (with-tab-history
    (bydi (partial-recall--remember
           (:always buffer-live-p)
           minibuffer-selected-window
           (:mock window-buffer :return (current-buffer))
           (:ignore partial-recall--mapped-buffer-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--remember)
      (bydi-was-called minibuffer-selected-window))))

(ert-deftest pr--handle-buffer--reclaims ()
  (with-tab-history
    (bydi (partial-recall--remember
           partial-recall--recollect
           (:always buffer-live-p)
           (:mock window-buffer :return (current-buffer))
           (:always partial-recall--mapped-buffer-p))

      (partial-recall--handle-buffer (current-buffer))
      (bydi-was-called partial-recall--recollect))))

(ert-deftest pr--on-create--sets-cdr ()
  (bydi ((:mock partial-recall--create-key :return "test"))

    (let ((tab '(current-tab (name . "test-tab") (explicit-name))))

      (partial-recall--on-create tab)

      (should (string= "test" (alist-get 'pr tab))))))

(ert-deftest pr--on-close ()
  (bydi (partial-recall--suppress)
    (with-tab-history :pre t

      (should (eq 1 (length (hash-table-keys partial-recall--table))))

      (partial-recall--on-close test-tab t)

      (should (eq 1 (length (hash-table-keys partial-recall--table))))

      (partial-recall--on-close test-tab nil)

      (should (eq 0 (length (hash-table-keys partial-recall--table))))

      (bydi-was-called partial-recall--suppress))))

(ert-deftest pr--on-find-file ()
  (bydi (partial-recall--schedule-buffer)

    (partial-recall--on-find-file)

    (bydi-was-called partial-recall--schedule-buffer)))

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
           partial-recall--maybe-forget-oldest-moment
           ring-extend)

      (partial-recall--remember (current-buffer))
      (bydi-was-called ring-extend))))

(ert-deftest pr-remember--reinforces-permanent ()
  (let ((partial-recall-buffer-limit 1))
    (with-tab-history :pre t
      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (partial-recall--implant)

        (partial-recall--remember another-temp)

        (should (eq 2 (partial-recall--update-count)))

        (kill-buffer another-temp)))))

(ert-deftest pr-remember--removes-impermanent ()
  (let ((partial-recall-buffer-limit 1))
    (with-tab-history :pre t
      (let ((another-temp (generate-new-buffer " *temp*" t)))

        (bydi ((:always partial-recall--memory-at-capacity-p)
               (:ignore partial-recall--should-extend-memory-p)
               partial-recall--suppress)
          (partial-recall--remember another-temp)

          (bydi-was-called partial-recall--suppress))

        (kill-buffer another-temp)))))

(ert-deftest pr-swap ()
  (let* ((partial-recall-buffer-limit 1)
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
  (bydi ((:sometimes partial-recall--reality-buffer-p)
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
  (with-tab-history
    (let ((count nil)
          (seconds '(6 8 10 12))
          (get-count (lambda ()
                       (let* ((reality (partial-recall--reality))
                              (moments (partial-recall--memory-ring reality))
                              (buffer (current-buffer))
                              (moment (ring-ref moments (partial-recall--moments-member moments buffer))))
                         (partial-recall--moment-update-count moment))))
          (partial-recall-buffer-limit 2)
          (another-temp (generate-new-buffer " *temp*" t)))

      (bydi ((:mock time-to-seconds :with (lambda () (pop seconds))))
        (partial-recall--remember (current-buffer))

        (setq count (funcall get-count))

        (partial-recall--remember another-temp)

        (should (partial-recall--reinforce (current-buffer)))
        (should-not (eq count (funcall get-count))))

      (kill-buffer another-temp))))

(ert-deftest pr-reclaim--reclaims-from-other ()
  (let ((seconds '(10 12))
        (partial-recall-reclaim-min-age 0)
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
  (with-tab-history :pre t

    (bydi (partial-recall--remember)

      (partial-recall--reclaim (current-buffer))

      (bydi-was-not-called partial-recall--remember))))

(ert-deftest pr-forget--forgets ()
  (with-tab-history :pre t

    (bydi (partial-recall--suppress)
      (partial-recall--forget (current-buffer) t)

      (bydi-was-called partial-recall--suppress))

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory)))

      (should (eq 0 (ring-length ring))))))

(ert-deftest pr-forget--clears-subconscious ()
  (with-tab-history :pre t :lifts t

    (let ((sub (partial-recall--subconscious)))

      (partial-recall--forget (current-buffer) t)

      (should (partial-recall--memory-buffer-p (current-buffer) sub))

      (partial-recall--forget (current-buffer))

      (should-not (partial-recall--memory-buffer-p (current-buffer) sub)))))

(ert-deftest pr-forget--shortens-extended-memory ()
  (let ((partial-recall-buffer-limit 2)
        (another-temp (generate-new-buffer " *temp*" t))
        (yet-another-temp (generate-new-buffer " *temp*" t)))

    (bydi (partial-recall--suppress)
      (with-tab-history :pre t
        (partial-recall--remember another-temp)
        (partial-recall--remember yet-another-temp)

        (should (eq (ring-size (partial-recall--memory-ring (partial-recall--reality)))
                    3))

        (partial-recall--forget another-temp)

        (should (eq (ring-size (partial-recall--memory-ring (partial-recall--reality)))
                    2))

        (partial-recall--forget yet-another-temp)

        (should (eq (ring-size (partial-recall--memory-ring (partial-recall--reality)))
                    2))

        (kill-buffer another-temp)
        (kill-buffer yet-another-temp)))))

(ert-deftest partial-recall--suppress--remembers ()
  (with-tab-history :pre t
    (partial-recall--forget (current-buffer) t)

    (should (eq (ring-length (partial-recall--memory-ring (gethash partial-recall--subconscious-key partial-recall--table)))
                1))))

(ert-deftest partial-recall--suppress--removes-permanence ()
  (with-tab-history :pre t
    (let ((moment (partial-recall--moment-from-buffer (current-buffer))))

      (partial-recall--implant (current-buffer))
      (should (partial-recall--moment-permanence moment))

      (partial-recall--forget (current-buffer) t)
      (should-not (partial-recall--moment-permanence moment)))))

(ert-deftest partial-recall--suppress--kills ()
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
  (with-tab-history :lifts t :pre t
    (let ((sub (gethash partial-recall--subconscious-key partial-recall--table)))

      (partial-recall--forget (current-buffer) t)

      (should (eq (ring-length (partial-recall--memory-ring sub)) 1))

      (partial-recall--lift (current-buffer))

      (should (eq (ring-length (partial-recall--memory-ring sub)) 0)))))

(ert-deftest pr--maybe-implant-moment ()
  (let ((partial-recall-auto-implant t)
        (partial-recall-auto-implant-threshold 1))
    (with-tab-history :pre t

      (bydi ((:spy partial-recall--maybe-implant-moment)
             (:spy partial-recall--implant))

        (partial-recall--reinforce (current-buffer))
        (partial-recall--reinforce (current-buffer))
        (partial-recall--reinforce (current-buffer))

        (bydi-was-called-n-times partial-recall--maybe-implant-moment 4)
        (bydi-was-called-n-times partial-recall--implant 1))

      (let ((moment (partial-recall--moment-from-buffer (current-buffer))))
        (should (partial-recall--moment-permanence moment))))))

(ert-deftest pr--clean-up-buffer ()
  (let ((partial-recall--last-checked (current-buffer)))

    (partial-recall--clean-up-buffer (current-buffer))

    (should-not partial-recall--last-checked))

  (bydi ((:mock window-buffer :return (current-buffer))
         delete-window)

    (partial-recall--clean-up-buffer (current-buffer))

    (bydi-was-called delete-window)))

;;; -- Conditionals

(ert-deftest pr--memory-buffer-p ()
  (with-tab-history :pre t

    (let ((memory (gethash (alist-get 'pr test-tab) partial-recall--table)))

      (should (partial-recall--memory-buffer-p (current-buffer) memory)))))

(ert-deftest pr-moment-buffer-p ()
  (with-tab-history :pre t

    (let* ((memory (gethash (alist-get 'pr test-tab) partial-recall--table))
           (ring (partial-recall--memory-ring memory))
           (moment (ring-ref ring 0)))

      (should (partial-recall--moment-buffer-p (current-buffer) moment)))))

(ert-deftest pr--mapped-buffer-p ()
  (with-tab-history :pre t

    (should (partial-recall--mapped-buffer-p (current-buffer)))))

(ert-deftest pr--reality-buffer-p ()
  (with-tab-history :pre t
    (should (partial-recall--reality-buffer-p (current-buffer)))))

(ert-deftest pr--reality-owns-buffer-p ()
  (with-tab-history :pre t
    (should (partial-recall--reality-owns-buffer-p (current-buffer)))))

(ert-deftest pr--should-extend-memory-p ()
  (let ((seconds '(10 11 12))
        (partial-recall-buffer-limit 1)
        (partial-recall-max-age 2))

    (with-tab-history
      (bydi ((:mock time-to-seconds :with (lambda (&rest _) (pop seconds))))

        (partial-recall--remember (current-buffer))

        (let ((memory (partial-recall--reality)))

          (should (partial-recall--should-extend-memory-p memory))
          (should-not (partial-recall--should-extend-memory-p memory)))))))

(ert-deftest pr--has-buffers-p ()
  (with-tab-history
    (should-not (partial-recall--has-buffers-p))

    (partial-recall--remember (current-buffer))

    (should (partial-recall--has-buffers-p))))

;; -- Utility

(ert-deftest pr--warn ()
  (bydi (display-warning)
    (partial-recall--warn "Testing")
    (bydi-was-called-with display-warning (list 'partial-recall "Testing" :warning))))

(ert-deftest pr--log ()
  (let ((partial-recall-log nil)
        (partial-recall-log-level 1))

    (should-not (partial-recall--log "test: %s %s" "one" "two"))

    (setq partial-recall-log t)

    (ert-with-message-capture messages
      (partial-recall--log "test: %s %s" "one" "two")

      (partial-recall--debug "test: %s" "three")

      (setq partial-recall-log-level 0)

      (partial-recall--debug "test: %s" "four")

      (should (string= messages "test: one two\ntest: four\n")))))

(ert-deftest pr--repr ()
  (bydi-with-mock ((:mock format-time-string :return "now")
                   (:mock partial-recall--name :return "test"))

    (let* ((partial-recall-buffer-limit 3)
           (memory (partial-recall--memory-create "test"))
           (buffer (get-buffer-create "test-print"))
           (moment (partial-recall--moment-create buffer)))

      (should (string= "#<memory test (0/3)>" (partial-recall--repr memory)))
      (should (string= "#<moment test-print (now)>" (partial-recall--repr moment)))
      (should-error (partial-recall--repr 'hello) :type 'user-error)

      (kill-buffer buffer))))

;;; -- Completion

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

(ert-deftest pr--complete-reality--inital-input ()
  (bydi (completing-read
         (:always partial-recall--reality-owns-buffer-p)
         (:mock partial-recall--mapped-buffers :return '(first second third))
         (:mock buffer-name :with bydi-rf)
         (:mock current-buffer :return 'second))

    (partial-recall--complete-reality "Some prompt: ")

    (bydi-was-called-with completing-read
      '("Some prompt: " ((first . first) (second . second) (third . third)) nil t second))))

(ert-deftest pr--complete-subconscious ()
  (let* ((partial-recall--table (make-hash-table))
         (buf (generate-new-buffer " *temp*" t))
         (name (buffer-name buf))
         (sub (partial-recall--subconscious)))

    (ring-insert (partial-recall--memory-ring sub)
                 (partial-recall--moment-create buf))

    (bydi-with-mock ((:mock completing-read :return name))

      (should (equal buf (partial-recall--complete-subconscious "Some prompt: "))))))

(ert-deftest pr--complete-any ()
  (bydi (completing-read
         (:mock buffer-list :return '(a b c d))
         (:mock buffer-name :with bydi-rf)
         (:mock buffer-file-name :with bydi-rf)
         (:mock partial-recall--mapped-buffers :return '(a c))
         (:mock partial-recall--mapped-buffer-p :with (lambda (it &rest _) (memq it '(a c))))
         (:mock current-buffer :return 'current))

    (partial-recall--complete-any "Some prompt: ")

    (bydi-was-called-with completing-read
      '("Some prompt: " ((b . b) (d . d)) nil t current))))

;;; -- Setup

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

      (bydi-toggle-sometimes)

      (partial-recall--fix-up-primary-tab)
      (bydi-was-called partial-recall--key)
      (bydi-was-called partial-recall--on-create))))

(ert-deftest pr--queue-fix-up ()
  (bydi (run-at-time)
    (partial-recall--queue-fix-up)

    (bydi-was-called-with run-at-time (list 1.0 nil #'partial-recall--fix-up-primary-tab))))

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
      (bydi-was-called-n-times add-hook 8)
      (bydi-was-called tab-bar-mode))))

(ert-deftest pr--teardown ()
  (bydi (remove-hook)

    (partial-recall-mode--teardown)

    (bydi-was-called-n-times remove-hook 8)))

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
           partial-recall--complete-subconscious
           partial-recall--complete-any
           partial-recall--implant
           partial-recall--lift
           partial-recall--remember
           switch-to-buffer)

      (call-interactively 'partial-recall-reinforce)
      (call-interactively 'partial-recall-reclaim)
      (call-interactively 'partial-recall-forget)
      (call-interactively 'partial-recall-switch-to-buffer)
      (call-interactively 'partial-recall-implant)
      (call-interactively 'partial-recall-lift)
      (call-interactively 'partial-recall-remember)

      (bydi-was-called partial-recall--reinforce)
      (bydi-was-called partial-recall--reclaim)
      (bydi-was-called partial-recall--forget)
      (bydi-was-called partial-recall--complete-dream)
      (bydi-was-called partial-recall--complete-reality)
      (bydi-was-called partial-recall--implant)
      (bydi-was-called partial-recall--lift)
      (bydi-was-called partial-recall--remember)
      (bydi-was-called-n-times switch-to-buffer 2))))

;;; Code:

;;; partial-recall-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:

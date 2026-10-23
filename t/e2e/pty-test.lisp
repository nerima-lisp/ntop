(in-package #:ntop.e2e.test)

(defmacro define-frame-cases (&body cases)
  `(cl-weave:it-each ,cases
                     "frame readiness for ~A"
                     (case-name started-at
                                last-output-at
                                now
                                first-frame-not-before-at
                                quiet-seconds
                                expected)
                     (declare (ignore case-name))
                     (cl-weave:expect
                      (ntop.e2e::frame-ready-p started-at
                                               last-output-at
                                               now
                                               first-frame-not-before-at
                                               quiet-seconds)
                      :to-be
                      expected)))

(cl-weave:describe "E2E PTY frame timing"
                   (cl-weave:it
                    "expands the frame case table into cl-weave cases"
                    (cl-weave:expect
                     (macroexpand-1
                      '(define-frame-cases
                        (:ready-at-boundaries 10 11 12 12 1 t)))
                     :to-equal
                     '(cl-weave:it-each ((:ready-at-boundaries 10 11 12 12 1 t))
                                        "frame readiness for ~A"
                                        (case-name started-at
                                                   last-output-at
                                                   now
                                                   first-frame-not-before-at
                                                   quiet-seconds
                                                   expected)
                                        (declare (ignore case-name))
                                        (cl-weave:expect
                                         (ntop.e2e::frame-ready-p started-at
                                                                  last-output-at
                                                                  now
                                                                  first-frame-not-before-at
                                                                  quiet-seconds)
                                         :to-be
                                         expected))))
                   (define-frame-cases
                    (:before-first-frame 10 11 12 121/10 1 nil)
                    (:before-quiet-interval 10 11 1199/100 11 1 nil)
                    (:at-both-boundaries 10 11 12 12 1 t)
                    (:output-before-start 10 9 12 12 0 nil)
                    (:no-output 10 nil 12 12 0 nil)
                    (:later-output-rests-briefly 10 119/10 12 11 1 nil)))

(cl-weave:describe "E2E PTY operation deadlines"
                   (cl-weave:it "expands the operation timeout macro"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e::with-e2e-operation-timeout
                                    ("probe" deadline 1d0)
                                    (values :completed)))
                                 :to-equal
                                 '(handler-case (sb-ext:with-timeout
                                                 (ntop.e2e::%ticks-to-seconds
                                                  (- deadline (ntop.e2e::%now)))
                                                 (values :completed))
                                    (sb-ext:timeout ()
                                      (ntop.e2e::%signal-timeout "probe" 1.0d0)))))
                   (cl-weave:it
                    "turns an operation deadline into an E2E timeout"
                    (let ((deadline
                           (+ (ntop.e2e::%now)
                              (ntop.e2e::%seconds-to-ticks 0.02d0)))
                          (caught nil))
                      (handler-case (ntop.e2e::with-e2e-operation-timeout
                                     ("probe" deadline 0.02d0)
                                     (sleep 0.08d0))
                        (ntop.e2e::e2e-timeout (condition)
                          (setf caught condition)))
                      (cl-weave:expect (typep caught 'ntop.e2e::e2e-timeout)
                                       :to-be
                                       t)
                      (cl-weave:expect (ntop.e2e::e2e-timeout-operation caught)
                                       :to-equal
                                       "probe"))))
(cl-weave:describe "E2E shared deadlines and process cleanup"
                   (cl-weave:it
                    "clamps calls to one measurement deadline"
                    (let* ((started 1000)
                           (deadline
                            (+ started
                               (ntop.e2e::%seconds-to-ticks 10d0)))
                           (ntop.e2e::*e2e-deadline* deadline)
                           (first
                            (ntop.e2e::%effective-timeout-seconds
                             30d0 "command" started))
                           (second
                            (ntop.e2e::%effective-timeout-seconds
                             30d0
                             "pty"
                             (+ started
                                (ntop.e2e::%seconds-to-ticks 7d0))))
                           (short
                            (ntop.e2e::%effective-timeout-seconds
                             2d0 "command" started)))
                      (cl-weave:expect first :to-equal 10)
                      (cl-weave:expect second :to-equal 3)
                      (cl-weave:expect short :to-equal 2)
                      (let ((expired nil)
                            (ntop.e2e::*e2e-deadline* started))
                        (handler-case
                            (ntop.e2e::%effective-timeout-seconds
                             1d0 "expired" started)
                          (ntop.e2e::e2e-timeout (condition)
                            (setf expired condition)))
                        (cl-weave:expect
                         (typep expired 'ntop.e2e::e2e-timeout)
                         :to-be
                         t))))
                   (cl-weave:it
 "starts session timing after launch while respecting the absolute deadline"
 (let* ((launch-started-at 1000)
        (session-started-at
         (+ launch-started-at
            (ntop.e2e::%seconds-to-ticks 7d0)))
        (ntop.e2e::*e2e-deadline*
         (+ launch-started-at
            (ntop.e2e::%seconds-to-ticks 30d0))))
   (multiple-value-bind (launch-deadline launch-timeout)
       (ntop.e2e::%deadline-from-start 12d0 "launch" launch-started-at)
     (multiple-value-bind (session-deadline session-timeout)
         (ntop.e2e::%deadline-from-start 12d0 "read" session-started-at)
       (cl-weave:expect launch-timeout :to-equal 12)
       (cl-weave:expect session-timeout :to-equal 12)
       (cl-weave:expect session-deadline
                        :to-equal
                        (+ session-started-at
                           (ntop.e2e::%seconds-to-ticks 12d0)))
       (cl-weave:expect (> session-deadline launch-deadline) :to-be t)))
   (let ((ntop.e2e::*e2e-deadline*
          (+ launch-started-at
             (ntop.e2e::%seconds-to-ticks 10d0))))
     (multiple-value-bind (session-deadline session-timeout)
         (ntop.e2e::%deadline-from-start 12d0 "read" session-started-at)
       (cl-weave:expect session-timeout :to-equal 3)
       (cl-weave:expect session-deadline
                        :to-equal
                        ntop.e2e::*e2e-deadline*)))))
                   (cl-weave:it
                    "passes the remaining deadline to host-kit"
                    (let* ((started (ntop.e2e::%now))
                           (deadline
                            (+ started
                               (ntop.e2e::%seconds-to-ticks 0.05d0)))
                           (ntop.e2e::*e2e-deadline* deadline)
                           (condition-type
                            (find-symbol "PROCESS-TIMEOUT" "HOST-KIT"))
                           (caught nil))
                      (handler-case
                          (ntop.e2e::%read-command-output
                           "/bin/sleep" (list "2") 10d0)
                        (error (condition)
                          (setf caught condition)))
                      (cl-weave:expect
                       (and condition-type
                            (typep caught condition-type))
                       :to-be
                       t)))
                   (cl-weave:it
                    "waits for the child process during session close"
                    (let* ((session
                            (ntop.e2e::launch-e2e-process
                             "/bin/sleep" (list "30") 24 80
                             :timeout-seconds 5d0))
                           (process
                            (ntop.e2e::e2e-session-process session)))
                      (ntop.e2e::close-e2e-session session)
                      (cl-weave:expect
                       (and
                        (ntop.e2e::e2e-session-closed-p session)
                        (not (sb-ext:process-alive-p process)))
                       :to-be
                       t))))

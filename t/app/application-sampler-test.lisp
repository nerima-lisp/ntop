(in-package #:ntop.test)
(cl-weave:describe "application sampler"
  (cl-weave:it
    "discards one warm-up result and keeps live samples on the fixed-rate grid"
    (let ((ticks 0)
          (collection-starts nil)
          (waits nil)
          (published nil)
          (collection-count 0)
          (stop-requested nil))
      (labels ((now ()
                 (/ ticks 10))
               (wait (seconds)
                 (let ((duration-ticks (round (* 10 seconds))))
                   (push duration-ticks waits)
                   (incf ticks duration-ticks)))
               (collect ()
                 (incf collection-count)
                 (push (now) collection-starts)
                 (incf ticks 3)
                 collection-count)
               (publish (sample)
                 (push sample published)
                 (when (= (length published) 3)
                   (setf stop-requested t)))
               (stop-p ()
                 stop-requested))
        (ntop.app.application:run-fixed-rate-sampler
         700 (function now) (function wait) (function collect)
         (function publish) (function stop-p))
        (let* ((starts (nreverse collection-starts))
               (t0 (second starts)))
          (cl-weave:expect
           starts
           :to-equal (quote (0 3/10 1 17/10)))
          (cl-weave:expect
           (rest starts)
           :to-equal (list t0 (+ t0 7/10) (+ t0 14/10)))
          (cl-weave:expect
           (nreverse waits)
           :to-equal (quote (4 4)))
          (cl-weave:expect
           (nreverse published)
           :to-equal (quote (2 3 4)))
          (cl-weave:expect
           collection-count
           :to-equal 4)))))

  (cl-weave:it "rejects zero and negative sampling rates"
    (dolist (rate (quote (0 -700)))
      (cl-weave:signals simple-error
        (ntop.app.application:run-fixed-rate-sampler
         rate
         (lambda () 0)
         (lambda (seconds) (declare (ignore seconds)))
         (lambda () nil)
         (lambda (sample) (declare (ignore sample)))
         (lambda () t)))))

  (cl-weave:it "returns unavailability reasons for all application features"
    (let* ((reasons
             (ntop.app.application::%make-application-unavailability-reasons
              :cpu :disabled
              :memory :permission-denied
              :gpu :not-present
              :network :unsupported-platform
              :disk :private-api-unavailable
              :temperature :read-failed
              :battery :disabled
              :process :not-present))
           (expected
             (quote ((:cpu :disabled)
                     (:memory :permission-denied)
                     (:gpu :not-present)
                     (:network :unsupported-platform)
                     (:disk :private-api-unavailable)
                     (:temperature :read-failed)
                     (:battery :disabled)
                     (:process :not-present)))))
      (dolist (entry expected)
        (cl-weave:expect
         (ntop.app.application:application-unavailability-reason
          reasons (first entry))
         :to-equal (second entry)))
      (cl-weave:expect
       (ntop.app.application:application-unavailability-reason nil :cpu)
       :to-be nil)
      (cl-weave:expect
       (handler-case
           (progn
             (ntop.app.application:application-unavailability-reason
              reasons :unknown)
             nil)
         (error () t))
       :to-be t)))

  (cl-weave:it "rejects invalid reasons for every application feature"
    (dolist (feature
             '(:cpu :memory :gpu :network :disk :temperature :battery :process))
      (cl-weave:signals
       type-error
       (apply
        (function ntop.app.application::%make-application-unavailability-reasons)
        (loop for key in
              '(:cpu :memory :gpu :network :disk :temperature :battery :process)
              append (list key (if (eq key feature) :invalid nil)))))))
)
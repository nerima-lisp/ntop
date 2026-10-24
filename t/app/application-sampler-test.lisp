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
  (cl-weave:it "keeps composing history while sampling during scroll"
    (let* ((composition
             (ntop.app.application:make-application-composition
              (ntop.shared.application:make-application-config)))
           (state (ntop.app.domain:make-app-state :selected-widget :network))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot composition))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (ticks 0)
           (collection-count 0)
           (published-count 0)
           (sampled-at 210d0)
           (stop-requested nil))
      (labels ((make-snapshot (time)
                 (ntop.app.domain:make-collection-snapshot
                  :sampled-at time
                  :network-sample
                  (ntop.network.domain:make-network-sample
                   :sampled-at time :interfaces nil)))
               (record-sample (time)
                 (funcall on-snapshot (make-snapshot time)))
               (visible-times ()
                 (mapcar
                  (function ntop.shared.domain:sample-record-sampled-at)
                  (ntop.app.application::%network-visible-samples composition))))
        (loop for time from 10d0 to 210d0 by 20d0
              do (record-sample time))
        (funcall on-action state
                 (ntop.app.domain:make-app-action
                  :kind :history-scroll :value :backward))
        (let ((visible-before (visible-times)))
          (cl-weave:expect visible-before
                           :to-equal (list 90d0 110d0 130d0 150d0))
          (labels ((now ()
                     (/ ticks 10))
                   (wait (seconds)
                     (incf ticks (round (* 10 seconds))))
                   (collect ()
                     (incf collection-count)
                     (incf sampled-at 20d0)
                     (make-snapshot sampled-at))
                   (publish (snapshot)
                     (funcall on-snapshot snapshot)
                     (incf published-count)
                     (when (= published-count 3)
                       (setf stop-requested t)))
                   (stop-p ()
                     stop-requested))
            (ntop.app.application:run-fixed-rate-sampler
             700 (function now) (function wait) (function collect)
             (function publish) (function stop-p))
            (let* ((store
                     (gethash
                      (list :network :window)
                      (ntop.app.application::application-composition-history-index
                       composition)))
                   (history
                     (ntop.shared.domain:history-slice
                      store 0 :start 0d0 :end 300d0))
                   (latest (aref history (1- (length history)))))
              (cl-weave:expect collection-count :to-equal 4)
              (cl-weave:expect published-count :to-equal 3)
              (cl-weave:expect (length history) :to-equal 14)
              (cl-weave:expect
               (ntop.shared.domain:history-sample-sampled-at latest)
               :to-equal 290d0)
              (cl-weave:expect (visible-times) :to-equal visible-before)
              (funcall on-action state
                       (ntop.app.domain:make-app-action :kind :return-to-now))
              (cl-weave:expect
               (visible-times)
               :to-equal (list 250d0 270d0 290d0))))))))

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
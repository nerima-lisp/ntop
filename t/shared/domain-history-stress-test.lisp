(in-package #:ntop.test)

(cl-weave:describe "retained time-series stress"
  (cl-weave:around-each (next)
    (let ((cl-weave:*property-seed* 20261024))
      (funcall next)))

  (cl-weave:it-property "ring length and order stay bounded for every append count"
      ((values
        (cl-weave:gen-tuple
         (cl-weave:gen-integer :min 0 :max 100))))
    (let* ((count (first values))
           (store (%history-test-store :retention 5d0
                                       :interval 1d0
                                       :window 5d0
                                       :minimum 1d0)))
      (loop for time from 0 below count
            do (%append-history-at store 0 time time))
      (let ((times (%history-times store 0 :start -1d0 :end 1000d0)))
        (cl-weave:expect
         (and (= (length times) (min count 6))
              (%strictly-increasing-p times)
              (if (zerop count)
                  (null times)
                  (= (car (last times)) (1- count))))
         :to-be t)))))
(cl-weave:describe "retained time-series boundary paths"
  (cl-weave:it "combines bounds across populated series"
    (let ((store (%history-test-store :series-count 2)))
      (%append-history-at store 0 5 50)
      (%append-history-at store 1 2 20)
      (multiple-value-bind (start end)
          (ntop.shared.domain:history-current-window store)
        (cl-weave:expect start :to-be 2d0)
        (cl-weave:expect end :to-be 5d0))))

  (cl-weave:it "rejects invalid samples, clocks, and explicit slice bounds"
    (let ((store (%history-test-store)))
      (cl-weave:expect
       (%signals-history-error-p
        (lambda ()
          (ntop.shared.domain:history-append store 0 :not-a-record 1)))
       :to-be t)
      (cl-weave:expect
       (%signals-history-error-p
        (lambda () (%append-history-at store 0 1 "not numeric")))
       :to-be t)
      (cl-weave:expect
       (%signals-history-error-p
        (lambda () (ntop.shared.domain:history-prune store "not a clock")))
       :to-be t)
      (cl-weave:expect
       (%signals-history-error-p
        (lambda () (ntop.shared.domain:history-slice store 0 :start 1)))
       :to-be t)
      (cl-weave:expect
       (%signals-history-error-p
        (lambda () (ntop.shared.domain:history-slice store 0 :start 2 :end 1)))
       :to-be t)))

  (cl-weave:it "reports pause state and resumes an unpaused or repeated pause"
    (let ((store (%history-test-store)))
      (cl-weave:expect (ntop.shared.domain:history-paused-p store) :to-be nil)
      (cl-weave:expect (ntop.shared.domain:history-resume store) :to-be nil)
      (%append-history-range store 0 0 5)
      (cl-weave:expect (ntop.shared.domain:history-pause store) :to-be t)
      (cl-weave:expect (ntop.shared.domain:history-pause store) :to-be t)
      (cl-weave:expect (ntop.shared.domain:history-paused-p store) :to-be t)
      (cl-weave:expect (ntop.shared.domain:history-resume store) :to-be t)
      (cl-weave:expect (ntop.shared.domain:history-paused-p store) :to-be nil)))

  (cl-weave:it "resumes live following after scrolling newer while paused"
    (let ((store (%history-test-store :retention 10d0 :window 3d0)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-pause store)
      (ntop.shared.domain:history-scroll-older store)
      (cl-weave:expect
       (ntop.shared.domain:history-scroll-newer store)
       :to-be 5d0)
      (%append-history-at store 0 6 6)
      (ntop.shared.domain:history-resume store)
      (cl-weave:expect (%history-window-end store) :to-be 6d0))))

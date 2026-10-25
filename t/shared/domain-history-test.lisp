(in-package #:ntop.test)

(defun %history-test-store
    (&key (retention 5d0) (interval 1d0) (window 3d0)
          (step 1d0) (minimum 1d0) (series-count 2))
  (ntop.shared.domain:make-history-store
   series-count
   :retention-seconds retention
   :sample-interval-seconds interval
   :initial-window-seconds window
   :zoom-step-seconds step
   :minimum-window-seconds minimum))

(defun %append-history-at (store series time value)
  (ntop.shared.domain:history-append
   store series
   (ntop.shared.domain:make-sample-record :sampled-at time)
   value))

(defun %append-history-range (store series from through)
  (loop for time from from to through
        do (%append-history-at store series time time)))

(defun %history-samples (store series &key start end)
  (if (and start end)
      (ntop.shared.domain:history-slice store series :start start :end end)
      (ntop.shared.domain:history-slice store series)))

(defun %history-times (store series &key start end)
  (map 'list #'ntop.shared.domain:history-sample-sampled-at
       (%history-samples store series :start start :end end)))

(defun %history-values (store series &key start end)
  (map 'list #'ntop.shared.domain:history-sample-value
       (%history-samples store series :start start :end end)))

(defun %history-window-start (store)
  (nth-value 0 (ntop.shared.domain:history-current-window store)))

(defun %history-window-end (store)
  (nth-value 1 (ntop.shared.domain:history-current-window store)))

(defun %signals-history-error-p (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error () t)))

(defun %strictly-increasing-p (values)
  (loop for tail on values
        while (cdr tail)
        always (< (first tail) (second tail))))

(cl-weave:describe "retained time-series history"
  (cl-weave:it "allocates the configured default retention capacity"
    (let ((store (ntop.shared.domain:make-history-store 22)))
      (cl-weave:expect
       (ntop.shared.domain:history-store-capacity store)
       :to-be 859)
      (cl-weave:expect
       (ntop.shared.domain:history-store-series-count store)
       :to-be 22)))

  (cl-weave:it "exposes its time-series configuration"
    (let ((store (ntop.shared.domain:make-history-store 3)))
      (cl-weave:expect
       (and (= 600d0
               (ntop.shared.domain:history-store-retention-seconds store))
            (= 0.7d0
               (ntop.shared.domain:history-store-sample-interval-seconds
                store))
            (= 15d0
               (ntop.shared.domain:history-store-zoom-step-seconds store))
            (= 60d0
               (ntop.shared.domain:history-store-window-width-seconds store)))
       :to-be t)))

  (cl-weave:it-each ((1 1 2)
                     (5 2 4)
                     (5 1 6)
                     (600 0.7 859))
      "uses a fixed capacity for retention ~A and interval ~A"
      (retention interval expected)
    (let ((store
            (ntop.shared.domain:make-history-store
             1 :retention-seconds retention
             :sample-interval-seconds interval)))
      (cl-weave:expect
       (ntop.shared.domain:history-store-capacity store)
       :to-be expected)))

  (cl-weave:it "rejects a zero series count"
    (cl-weave:expect
     (%signals-history-error-p
      (lambda ()
        (ntop.shared.domain:make-history-store 0)))
     :to-be t))
  (cl-weave:it "retains history when actual samples arrive faster than configured"
    (let ((store
            (ntop.shared.domain:make-history-store
             1 :retention-seconds 1d0
             :sample-interval-seconds 1d0)))
      (%append-history-at store 0 0d0 1)
      (%append-history-at store 0 0.4d0 2)
      (%append-history-at store 0 0.8d0 3)
      (cl-weave:expect
       (equalp (%history-times store 0) '(0d0 0.4d0 0.8d0))
       :to-be t)
      (cl-weave:expect
       (> (ntop.shared.domain:history-store-capacity store) 2)
       :to-be t)))

  (cl-weave:it "rejects nonpositive retention"
    (cl-weave:expect
     (%signals-history-error-p
      (lambda ()
        (ntop.shared.domain:make-history-store
         1 :retention-seconds 0d0)))
     :to-be t))

  (cl-weave:it "rejects a nonpositive sample interval"
    (cl-weave:expect
     (%signals-history-error-p
      (lambda ()
        (ntop.shared.domain:make-history-store
         1 :sample-interval-seconds -1d0)))
     :to-be t))

  (cl-weave:it "starts with an empty current window"
    (let ((store (%history-test-store)))
      (cl-weave:expect
       (equal
        (multiple-value-list
         (ntop.shared.domain:history-current-window store))
        '(nil nil))
       :to-be t)))

  (cl-weave:it "returns an empty copied slice before samples arrive"
    (let ((store (%history-test-store)))
      (cl-weave:expect
       (zerop (length (ntop.shared.domain:history-slice store 0)))
       :to-be t)))

  (cl-weave:it "stores sample-record timestamps and numeric values"
    (let ((store (%history-test-store)))
      (%append-history-at store 0 2 21)
      (%append-history-at store 0 3 31)
      (cl-weave:expect
       (equalp (%history-times store 0 :start 0d0 :end 5d0)
               '(2d0 3d0))
       :to-be t)
      (cl-weave:expect
       (equalp (%history-values store 0 :start 0d0 :end 5d0)
               '(21d0 31d0))
       :to-be t)))

  (cl-weave:it "stores missing measurements as time-stamped gaps"
    (let ((store (%history-test-store)))
      (%append-history-at store 0 1 nil)
      (%append-history-at store 0 2 4)
      (cl-weave:expect
       (equalp (%history-values store 0 :start 0d0 :end 5d0)
               '(nil 4d0))
       :to-be t)))

  (cl-weave:it "replaces a duplicate time point instead of growing the ring"
    (let ((store (%history-test-store)))
      (%append-history-at store 0 1 4)
      (%append-history-at store 0 1 8)
      (cl-weave:expect
       (and (equalp (%history-times store 0 :start 0d0 :end 5d0)
                    '(1d0))
            (equalp (%history-values store 0 :start 0d0 :end 5d0)
                    '(8d0)))
       :to-be t)))

  (cl-weave:it "keeps wrapped ring samples in chronological order"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 6)
      (cl-weave:expect
       (equalp (%history-times store 0 :start -1d0 :end 10d0)
               '(1d0 2d0 3d0 4d0 5d0 6d0))
       :to-be t)))

  (cl-weave:it "keeps series data independent"
    (let ((store (%history-test-store)))
      (%append-history-at store 0 1 10)
      (%append-history-at store 1 2 20)
      (cl-weave:expect
       (and (equalp (%history-values store 0 :start 0d0 :end 5d0)
                    '(10d0))
            (equalp (%history-values store 1 :start 0d0 :end 5d0)
                    '(20d0)))
       :to-be t)))

  (cl-weave:it "includes both bounds when slicing an explicit window"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (cl-weave:expect
       (equalp (%history-times store 0 :start 2d0 :end 4d0)
               '(2d0 3d0 4d0))
       :to-be t)))

  (cl-weave:it "returns fresh sample vectors to readers"
    (let* ((store (%history-test-store))
           (first-slice nil))
      (%append-history-at store 0 1 1)
      (setf first-slice (ntop.shared.domain:history-slice store 0))
      (setf (aref first-slice 0) nil)
      (cl-weave:expect
       (equalp (%history-values store 0 :start 0d0 :end 5d0)
               '(1d0))
       :to-be t)))

  (cl-weave:it "retains samples exactly on the pruning boundary"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-prune store 5d0)
      (cl-weave:expect
       (equalp (%history-times store 0 :start -1d0 :end 10d0)
               '(0d0 1d0 2d0 3d0 4d0 5d0))
       :to-be t)))

  (cl-weave:it "prunes samples strictly older than the retention boundary"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-prune store (+ 5d0 0.000001d0))
      (cl-weave:expect
       (equalp (%history-times store 0 :start -1d0 :end 10d0)
               '(1d0 2d0 3d0 4d0 5d0))
       :to-be t)))

  (cl-weave:it "reports the number of pruned points across all series"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (%append-history-range store 1 0 5)
      (cl-weave:expect
       (ntop.shared.domain:history-prune store 10d0)
       :to-be 10)))

  (cl-weave:it "does not reinsert samples that already expired"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-prune store 10d0)
      (cl-weave:expect
       (%append-history-at store 1 4 400)
       :to-be :expired)
      (cl-weave:expect
       (length (%history-times store 1 :start -1d0 :end 20d0))
       :to-be 0)))

  (cl-weave:it "rejects decreasing timestamps within a series"
    (let ((store (%history-test-store)))
      (%append-history-at store 0 3 30)
      (cl-weave:expect
       (%signals-history-error-p
        (lambda () (%append-history-at store 0 2 20)))
       :to-be t)))

  (cl-weave:it "rejects a decreasing pruning clock"
    (let ((store (%history-test-store)))
      (ntop.shared.domain:history-prune store 10d0)
      (cl-weave:expect
       (%signals-history-error-p
        (lambda () (ntop.shared.domain:history-prune store 9d0)))
       :to-be t)))

  (cl-weave:it "rejects a series index outside the fixed store"
    (let ((store (%history-test-store :series-count 1)))
      (cl-weave:expect
       (%signals-history-error-p
        (lambda () (ntop.shared.domain:history-slice store 1)))
       :to-be t)))

  (cl-weave:it "grows all rings when arrivals exceed configured capacity"
    (let ((store (%history-test-store :retention 1d0
                                      :interval 1d0
                                      :window 1d0
                                      :minimum 1d0)))
      (%append-history-at store 0 0d0 0)
      (%append-history-at store 0 0.5d0 1)
      (%append-history-at store 1 0d0 10)
      (%append-history-at store 1 0.5d0 11)
      (%append-history-at store 0 1d0 2)
      (%append-history-at store 1 1d0 12)
      (cl-weave:expect
       (equalp (%history-times store 0 :start -1d0 :end 2d0)
               '(0d0 0.5d0 1d0))
       :to-be t)
      (cl-weave:expect
       (equalp (%history-times store 1 :start -1d0 :end 2d0)
               '(0d0 0.5d0 1d0))
       :to-be t)
      (cl-weave:expect
       (ntop.shared.domain:history-store-capacity store)
       :to-be 4)))

  (cl-weave:it "follows the newest sample while in current mode"
    (let ((store (%history-test-store)))
      (%append-history-at store 0 1 1)
      (%append-history-at store 0 2 2)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 2d0)
      (%append-history-at store 0 3 3)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 3d0)))

  (cl-weave:it "scrolls older by exactly one current window width"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (cl-weave:expect
       (ntop.shared.domain:history-scroll-older store)
       :to-be 2d0)))

  (cl-weave:it "scrolls newer by exactly one current window width"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-scroll-older store)
      (cl-weave:expect
       (ntop.shared.domain:history-scroll-newer store)
       :to-be 5d0)))

  (cl-weave:it "clamps older scrolling at the oldest retained timestamp"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (loop repeat 5
            do (ntop.shared.domain:history-scroll-older store))
      (cl-weave:expect
       (%history-window-end store)
       :to-be 0d0)))

  (cl-weave:it "returns to following live samples at the newer edge"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-scroll-older store)
      (ntop.shared.domain:history-scroll-newer store)
      (%append-history-at store 0 6 6)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 6d0)))

  (cl-weave:it "does not move an explicitly scrolled endpoint on arrival"
    (let ((store (%history-test-store :retention 10d0 :window 2d0)))
      (%append-history-range store 0 0 10)
      (ntop.shared.domain:history-scroll-older store)
      (%append-history-at store 0 11 11)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 8d0)
      (cl-weave:expect
       (equalp (%history-times store 0) '(6d0 7d0 8d0))
       :to-be t)))

  (cl-weave:it "clamps a scrolled endpoint when pruning passes it"
    (let ((store (%history-test-store :retention 10d0
                                      :window 1d0 :step 1d0)))
      (%append-history-range store 0 0 10)
      (loop repeat 6
            do (ntop.shared.domain:history-scroll-older store))
      (ntop.shared.domain:history-prune store 15d0)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 5d0)))

  (cl-weave:it "G returns a live view to the current sample"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-scroll-older store)
      (ntop.shared.domain:history-go-current store)
      (%append-history-at store 0 6 6)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 6d0)))

  (cl-weave:it "zooms in without moving a scrolled endpoint"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-scroll-older store)
      (ntop.shared.domain:history-zoom-in store)
      (cl-weave:expect
       (and (= 2d0 (%history-window-end store))
            (= -0d0 (%history-window-start store))
            (= 2d0
               (ntop.shared.domain:history-store-window-width-seconds store)))
       :to-be t)))

  (cl-weave:it "clamps zoom-in at the minimum window width"
    (let ((store (%history-test-store)))
      (ntop.shared.domain:history-zoom-in store)
      (ntop.shared.domain:history-zoom-in store)
      (ntop.shared.domain:history-zoom-in store)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds store)
       :to-be 1d0)))

  (cl-weave:it "zooms out by one step and preserves the endpoint"
    (let ((store (%history-test-store)))
      (%append-history-range store 0 0 5)
      (ntop.shared.domain:history-scroll-older store)
      (ntop.shared.domain:history-zoom-out store)
      (cl-weave:expect
       (and (= 2d0 (%history-window-end store))
            (= -2d0 (%history-window-start store))
            (= 4d0
               (ntop.shared.domain:history-store-window-width-seconds store)))
       :to-be t)))

  (cl-weave:it "clamps zoom-out at the retention width"
    (let ((store (%history-test-store)))
      (ntop.shared.domain:history-zoom-out store)
      (ntop.shared.domain:history-zoom-out store)
      (ntop.shared.domain:history-zoom-out store)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds store)
       :to-be 5d0)))

  (cl-weave:it "clamps an explicitly selected width into supported bounds"
    (let ((store (%history-test-store)))
      (cl-weave:expect
       (ntop.shared.domain:history-set-window-width store 99d0)
       :to-be 5d0)))

  (cl-weave:it "resets zoom to its configured initial width"
    (let ((store (%history-test-store)))
      (ntop.shared.domain:history-set-window-width store 1d0)
      (ntop.shared.domain:history-reset-zoom store)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds store)
       :to-be 3d0)))

  (cl-weave:it "freezes the displayed endpoint and points when paused"
    (let ((store (%history-test-store :retention 10d0 :window 3d0)))
      (%append-history-range store 0 0 10)
      (let ((before (%history-times store 0)))
        (ntop.shared.domain:history-pause store)
        (%append-history-range store 0 11 20)
        (cl-weave:expect
         (and (ntop.shared.domain:history-paused-p store)
              (= 10d0 (%history-window-end store))
              (equalp (%history-times store 0) before))
         :to-be t))))

  (cl-weave:it "allows scrolling inside paused history"
    (let ((store (%history-test-store :retention 10d0 :window 2d0)))
      (%append-history-range store 0 0 10)
      (ntop.shared.domain:history-pause store)
      (ntop.shared.domain:history-scroll-older store)
      (cl-weave:expect
       (and (= 8d0 (%history-window-end store))
            (equalp (%history-times store 0) '(6d0 7d0 8d0)))
       :to-be t)))

  (cl-weave:it "keeps a pause snapshot after live slots are overwritten"
    (let ((store (%history-test-store :retention 10d0 :window 3d0)))
      (%append-history-range store 0 0 10)
      (ntop.shared.domain:history-pause store)
      (let ((paused-view (%history-times store 0)))
        (%append-history-range store 0 11 20)
        (cl-weave:expect
         (and (equalp (%history-times store 0) paused-view)
              (= 10d0 (%history-window-end store)))
         :to-be t)
        (ntop.shared.domain:history-resume store)
        (cl-weave:expect
         (equalp (%history-times store 0 :start -1d0 :end 30d0)
                 '(10d0 11d0 12d0 13d0 14d0
                   15d0 16d0 17d0 18d0 19d0 20d0))
         :to-be t)
        (cl-weave:expect
         (= 20d0 (%history-window-end store))
         :to-be t))))

  (cl-weave:it "G selects the frozen latest point and resumes following live data"
    (let ((store (%history-test-store :retention 10d0 :window 3d0)))
      (%append-history-range store 0 0 10)
      (ntop.shared.domain:history-pause store)
      (%append-history-at store 0 11 11)
      (ntop.shared.domain:history-go-current store)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 10d0)
      (ntop.shared.domain:history-resume store)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 11d0)))

  (cl-weave:it "clamps a paused older view into live retained history on resume"
    (let ((store (%history-test-store :retention 10d0 :window 2d0)))
      (%append-history-range store 0 0 10)
      (loop repeat 4
            do (ntop.shared.domain:history-scroll-older store))
      (ntop.shared.domain:history-pause store)
      (%append-history-range store 0 11 20)
      (ntop.shared.domain:history-resume store)
      (cl-weave:expect
       (%history-window-end store)
       :to-be 10d0))))



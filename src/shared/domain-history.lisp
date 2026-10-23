(in-package #:ntop.shared.domain)

(defstruct (history-sample
            (:constructor %make-history-sample (sampled-at value))
            (:copier nil))
  (sampled-at 0d0 :type double-float)
  (value nil :type (or null double-float)))

(defstruct (%history-ring
            (:constructor %make-history-ring
                (timestamps values present head count last-time)))
  timestamps
  values
  present
  (head 0 :type fixnum)
  (count 0 :type fixnum)
  last-time)

(defstruct (history-store
            (:constructor %make-history-store
                (series retention sample-interval capacity zoom-step
                 minimum-window initial-window window-width)))
  series
  (retention 0d0 :type double-float)
  (sample-interval 0d0 :type double-float)
  (capacity 0 :type fixnum)
  (zoom-step 0d0 :type double-float)
  (minimum-window 0d0 :type double-float)
  (initial-window 0d0 :type double-float)
  (window-width 0d0 :type double-float)
  view-endpoint
  paused-rings
  (resume-follows-p nil)
  retention-clock)

(defun %positive-seconds (value name)
  (unless (and (realp value) (plusp value))
    (error "~A must be a positive real number." name))
  (coerce value 'double-float))

(defun %make-ring (capacity)
  (%make-history-ring
   (make-array capacity :element-type 'double-float :initial-element 0d0)
   (make-array capacity :element-type 'double-float :initial-element 0d0)
   (make-array capacity :element-type 'bit :initial-element 0)
   0 0 nil))

(defun make-history-store
    (series-count
     &key
       (retention-seconds 600d0)
       (sample-interval-seconds 0.7d0)
       (initial-window-seconds 60d0)
       (zoom-step-seconds 15d0)
       (minimum-window-seconds 30d0))
  (unless (and (integerp series-count) (plusp series-count))
    (error "SERIES-COUNT must be a positive integer."))
  (let* ((retention (%positive-seconds retention-seconds
                                        "RETENTION-SECONDS"))
         (sample-interval (%positive-seconds sample-interval-seconds
                                              "SAMPLE-INTERVAL-SECONDS"))
         (zoom-step (%positive-seconds zoom-step-seconds
                                       "ZOOM-STEP-SECONDS"))
         (minimum (%positive-seconds minimum-window-seconds
                                     "MINIMUM-WINDOW-SECONDS"))
         (initial (%positive-seconds initial-window-seconds
                                     "INITIAL-WINDOW-SECONDS"))
         (capacity (1+ (ceiling retention sample-interval)))
         (minimum-window (min minimum retention))
         (initial-window (max minimum-window (min retention initial))))
    (%make-history-store
     (map 'vector (lambda (ignore)
                    (declare (ignore ignore))
                    (%make-ring capacity))
          (make-array series-count))
     retention sample-interval capacity zoom-step
     minimum-window initial-window initial-window)))

(defun history-store-series-count (store)
  (length (history-store-series store)))

(defun history-store-retention-seconds (store)
  (history-store-retention store))

(defun history-store-sample-interval-seconds (store)
  (history-store-sample-interval store))

(defun history-store-zoom-step-seconds (store)
  (history-store-zoom-step store))

(defun history-store-window-width-seconds (store)
  (history-store-window-width store))

(defun history-paused-p (store)
  (not (null (history-store-paused-rings store))))

(defun %ring-index (ring offset)
  (mod (+ (%history-ring-head ring) offset)
       (length (%history-ring-timestamps ring))))

(defun %ring-oldest-time (ring)
  (when (plusp (%history-ring-count ring))
    (aref (%history-ring-timestamps ring)
          (%history-ring-head ring))))

(defun %ring-newest-time (ring)
  (when (plusp (%history-ring-count ring))
    (aref (%history-ring-timestamps ring)
          (%ring-index ring (1- (%history-ring-count ring))))))

(defun %bounds-for-rings (rings)
  (let ((oldest nil)
        (newest nil))
    (loop for ring across rings
          for ring-oldest = (%ring-oldest-time ring)
          for ring-newest = (%ring-newest-time ring)
          when ring-oldest
            do (setf oldest (if oldest (min oldest ring-oldest) ring-oldest)
                     newest (if newest (max newest ring-newest) ring-newest)))
    (values oldest newest)))

(defun %current-rings (store)
  (or (history-store-paused-rings store)
      (history-store-series store)))

(defun %source-bounds (store)
  (%bounds-for-rings (%current-rings store)))

(defun %clamp-live-endpoint (store)
  (unless (history-store-paused-rings store)
    (let ((endpoint (history-store-view-endpoint store)))
      (when endpoint
        (multiple-value-bind (oldest newest)
            (%bounds-for-rings (history-store-series store))
          (setf (history-store-view-endpoint store)
                (and oldest (max oldest (min newest endpoint)))))))))

(defun %prune-ring (ring cutoff)
  (let ((removed 0)
        (capacity (length (%history-ring-timestamps ring))))
    (loop while (and (plusp (%history-ring-count ring))
                     (< (%ring-oldest-time ring) cutoff))
          do (setf (%history-ring-head ring)
                   (mod (1+ (%history-ring-head ring)) capacity))
             (decf (%history-ring-count ring))
             (incf removed))
    removed))

(defun %prune-rings (rings cutoff)
  (loop for ring across rings
        sum (%prune-ring ring cutoff)))

(defun %copy-ring (ring)
  (let* ((capacity (length (%history-ring-timestamps ring)))
         (copy (%make-ring capacity)))
    (setf (%history-ring-last-time copy) (%history-ring-last-time ring))
    (loop for offset below (%history-ring-count ring)
          for source = (%ring-index ring offset)
          do (setf (aref (%history-ring-timestamps copy) offset)
                   (aref (%history-ring-timestamps ring) source)
                   (aref (%history-ring-values copy) offset)
                   (aref (%history-ring-values ring) source)
                   (sbit (%history-ring-present copy) offset)
                   (sbit (%history-ring-present ring) source)))
    (setf (%history-ring-count copy) (%history-ring-count ring))
    copy))

(defun %copy-rings (rings)
  (map 'vector #'%copy-ring rings))

(defun %ensure-series-index (store series-index)
  (unless (and (integerp series-index)
               (<= 0 series-index)
               (< series-index (history-store-series-count store)))
    (error "SERIES-INDEX ~S is outside the history store." series-index)))

(defun %store-ring-sample (ring timestamp value)
  (let* ((count (%history-ring-count ring))
         (capacity (length (%history-ring-timestamps ring)))
         (present (if value 1 0))
         (stored-value (if value (coerce value 'double-float) 0d0)))
    (if (and (plusp count)
             (= timestamp (%history-ring-last-time ring)))
        (let ((index (%ring-index ring (1- count))))
          (setf (aref (%history-ring-values ring) index) stored-value
                (sbit (%history-ring-present ring) index) present))
        (progn
          (when (>= count capacity)
            (error "History capacity exhausted before retention pruning."))
          (let ((index (%ring-index ring count)))
            (setf (aref (%history-ring-timestamps ring) index) timestamp
                  (aref (%history-ring-values ring) index) stored-value
                  (sbit (%history-ring-present ring) index) present
                  (%history-ring-count ring) (1+ count)))))
    (setf (%history-ring-last-time ring) timestamp)
    (%make-history-sample timestamp (if value stored-value nil))))

(defun history-append (store series-index sample-record value)
  (unless (typep sample-record 'sample-record)
    (error "SAMPLE-RECORD must satisfy the shared sample-record contract."))
  (unless (typep value 'measurement-value)
    (error "VALUE must be a real number or NIL."))
  (let ((timestamp
         (coerce (sample-record-sampled-at sample-record) 'double-float)))
    (%ensure-series-index store series-index)
    (let* ((ring (aref (history-store-series store) series-index))
           (last-time (%history-ring-last-time ring))
           (now (if (history-store-retention-clock store)
                    (max timestamp (history-store-retention-clock store))
                  timestamp))
           (cutoff (- now (history-store-retention store))))
      (when (and last-time (< timestamp last-time))
        (error "Samples for one series must have nondecreasing timestamps."))
      (%prune-rings (history-store-series store) cutoff)
      (setf (history-store-retention-clock store) now)
      (if (< timestamp cutoff)
          (progn
            (%clamp-live-endpoint store)
            :expired)
        (let ((result (%store-ring-sample ring timestamp value)))
          (%clamp-live-endpoint store)
          result)))))

(defun history-prune (store monotonic-seconds)
  (unless (realp monotonic-seconds)
    (error "MONOTONIC-SECONDS must be real."))
  (let ((now (coerce monotonic-seconds 'double-float)))
    (when (and (history-store-retention-clock store)
               (< now (history-store-retention-clock store)))
      (error "History pruning time must be nondecreasing."))
    (setf (history-store-retention-clock store) now)
    (prog1
        (%prune-rings (history-store-series store)
                      (- now (history-store-retention store)))
      (%clamp-live-endpoint store))))

(defun history-pause (store)
  (unless (history-store-paused-rings store)
    (multiple-value-bind (oldest newest)
        (%bounds-for-rings (history-store-series store))
      (declare (ignore oldest))
      (setf (history-store-paused-rings store)
            (%copy-rings (history-store-series store))
            (history-store-resume-follows-p store)
            (null (history-store-view-endpoint store))
            (history-store-view-endpoint store) newest)))
  (not (null (history-store-paused-rings store))))

(defun history-resume (store)
  (when (history-store-paused-rings store)
    (setf (history-store-paused-rings store) nil)
    (if (history-store-resume-follows-p store)
        (setf (history-store-view-endpoint store) nil)
      (%clamp-live-endpoint store))
    (setf (history-store-resume-follows-p store) nil)
    t))

(defun history-scroll-older (store)
  (multiple-value-bind (oldest newest) (%source-bounds store)
    (when newest
      (let* ((endpoint (or (history-store-view-endpoint store) newest))
             (next (max oldest
                        (- endpoint (history-store-window-width store)))))
        (setf (history-store-view-endpoint store) next
              (history-store-resume-follows-p store) nil)
        next))))

(defun history-scroll-newer (store)
  (multiple-value-bind (oldest newest) (%source-bounds store)
    (declare (ignore oldest))
    (when newest
      (let* ((endpoint (or (history-store-view-endpoint store) newest))
             (next (min newest
                        (+ endpoint (history-store-window-width store)))))
        (setf (history-store-view-endpoint store) next)
        (when (= next newest)
          (when (history-store-paused-rings store)
            (setf (history-store-resume-follows-p store) t))
          (unless (history-store-paused-rings store)
            (setf (history-store-view-endpoint store) nil)))
        next))))

(defun history-go-current (store)
  (multiple-value-bind (oldest newest) (%source-bounds store)
    (declare (ignore oldest))
    (setf (history-store-view-endpoint store)
          (if (history-store-paused-rings store) newest nil)
          (history-store-resume-follows-p store)
          (not (null (history-store-paused-rings store))))
    newest))

(defun history-current-window (store)
  (multiple-value-bind (oldest newest) (%source-bounds store)
    (declare (ignore oldest))
    (let ((endpoint (or (history-store-view-endpoint store) newest)))
      (if endpoint
          (values (- endpoint (history-store-window-width store))
                  endpoint)
        (values nil nil)))))

(defun history-set-window-width (store width)
  (let ((requested (%positive-seconds width "WINDOW-WIDTH")))
    (setf (history-store-window-width store)
          (max (history-store-minimum-window store)
               (min (history-store-retention store) requested)))))

(defun history-zoom-in (store)
  (setf (history-store-window-width store)
        (max (history-store-minimum-window store)
             (- (history-store-window-width store)
                (history-store-zoom-step store)))))

(defun history-zoom-out (store)
  (setf (history-store-window-width store)
        (min (history-store-retention store)
             (+ (history-store-window-width store)
                (history-store-zoom-step store)))))

(defun history-reset-zoom (store)
  (setf (history-store-window-width store)
        (history-store-initial-window store)))

(defun history-slice (store series-index &key start end)
  (%ensure-series-index store series-index)
  (let* ((rings (%current-rings store))
         (ring (aref rings series-index))
         (from start)
         (through end))
    (when (and (null from) (null through))
      (multiple-value-bind (oldest newest) (%source-bounds store)
        (declare (ignore oldest))
        (setf through (or (history-store-view-endpoint store) newest))
        (when through
          (setf from (- through (history-store-window-width store))))))
    (when (not (eq (null from) (null through)))
      (error "START and END must either both be supplied or both be omitted."))
    (when (and from through (> from through))
      (error "START must not be later than END."))
    (if (null from)
        #()
      (let ((samples (make-array 0 :adjustable t :fill-pointer 0)))
        (loop for offset below (%history-ring-count ring)
              for index = (%ring-index ring offset)
              for timestamp = (aref (%history-ring-timestamps ring) index)
              when (<= from timestamp through)
              do (vector-push-extend
                  (%make-history-sample
                   timestamp
                   (if (= 1 (sbit (%history-ring-present ring) index))
                       (aref (%history-ring-values ring) index)
                     nil))
                  samples))
        (coerce samples 'simple-vector)))))



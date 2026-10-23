(in-package #:ntop.memory.infrastructure)

(defstruct (memory-collector
            (:constructor %make-memory-collector (clock enable-cache-memory-p))
            (:copier nil))
  clock
  enable-cache-memory-p)

(defun make-memory-collector (&key clock (enable-cache-memory-p nil))
  (unless clock
    (error "A monotonic clock is required."))
  (check-type enable-cache-memory-p boolean)
  (%make-memory-collector clock enable-cache-memory-p))

(defparameter +darwin-vm-statistics64-fields+
  '((:free-count . 0)
    (:active-count . 4)
    (:inactive-count . 8)
    (:wire-count . 12)
    (:purgeable-count . 88)
    (:speculative-count . 92)
    (:compressor-page-count . 128)
    (:internal-page-count . 140)))

(defun decode-darwin-vm-statistics64 (octets)
  (when (and (typep octets '(vector (unsigned-byte 8)))
             (= (length octets) 152))
    (loop for (key . offset) in +darwin-vm-statistics64-fields+
          append (list key
                       (ntop.platform.darwin:decode-little-endian-unsigned
                        octets :start offset :end (+ offset 4))))))

(defparameter +darwin-swapusage-fields+
  '((:total-bytes . 0)
    (:available-bytes . 8)))

(defun decode-darwin-swapusage (octets)
  (when (and (typep octets '(vector (unsigned-byte 8)))
             (= (length octets) 32))
    (loop for (key . offset) in +darwin-swapusage-fields+
          append (list key
                       (ntop.platform.darwin:decode-little-endian-unsigned
                        octets :start offset :end (+ offset 8))))))

(defun %memory-counter-p (value)
  (and (integerp value) (not (minusp value))))

(defun %darwin-cache-used-bytes (page-counts page-size)
  (let* ((free-pages (getf page-counts :free-count))
         (speculative-pages (getf page-counts :speculative-count))
         (available-bytes
           (* (+ (getf page-counts :active-count)
                 (getf page-counts :inactive-count)
                 free-pages)
              page-size))
         (free-bytes
           (* (max 0 (- free-pages speculative-pages)) page-size)))
    (ntop.memory.domain:memory-cache-used-bytes available-bytes free-bytes)))

(defun %darwin-memory-sample-from-data
    (sampled-at statistics page-size memory-total swapusage enable-cache-memory-p)
  (let* ((page-counts (decode-darwin-vm-statistics64 statistics))
         (swap-data (decode-darwin-swapusage swapusage)))
    (when (and page-counts swap-data
               (%memory-counter-p page-size) (plusp page-size)
               (%memory-counter-p memory-total))
      (let* ((ram-used
               (ntop.memory.domain:memory-used-bytes-from-pages
                (getf page-counts :internal-page-count)
                (getf page-counts :purgeable-count)
                (getf page-counts :wire-count)
                (getf page-counts :compressor-page-count)
                page-size))
             (swap-total (getf swap-data :total-bytes))
             (swap-available (getf swap-data :available-bytes))
             (swap-used (when (<= swap-available swap-total)
                          (- swap-total swap-available)))
             (cache-used
               (when enable-cache-memory-p
                 (%darwin-cache-used-bytes page-counts page-size))))
        (when (and (%memory-counter-p ram-used)
                   swap-used
                   (or (not enable-cache-memory-p)
                       (%memory-counter-p cache-used)))
          (ntop.memory.domain:make-memory-sample
           :sampled-at sampled-at
           :ram-used-bytes ram-used
           :ram-total-bytes memory-total
           :swap-used-bytes swap-used
           :swap-total-bytes swap-total
           :cache-used-bytes cache-used
           :cache-total-bytes (and enable-cache-memory-p memory-total)))))))

(defun %decode-darwin-sysctl-integer (octets)
  (when (and (typep octets '(vector (unsigned-byte 8)))
             (member (length octets) '(4 8)))
    (ntop.platform.darwin:decode-little-endian-unsigned octets)))

#+darwin
(defun %deliver-darwin-memory-sample
    (sampled-at statistics memory-total page-size collector on-sample on-unavailable)
  (ntop.platform.darwin:sysctl-read-by-name
   "vm.swapusage"
   (lambda (swapusage)
     (let ((sample
             (%darwin-memory-sample-from-data
              sampled-at statistics page-size memory-total swapusage
              (memory-collector-enable-cache-memory-p collector))))
       (if sample
           (funcall on-sample sample)
           (funcall on-unavailable :read-failed))))
   on-unavailable))

#+darwin
(defun %read-darwin-memory-page-size
    (sampled-at statistics memory-total collector on-sample on-unavailable)
  (ntop.platform.darwin:sysctl-read-by-name
   "hw.pagesize"
   (lambda (octets)
     (let ((page-size (%decode-darwin-sysctl-integer octets)))
       (if (%memory-counter-p page-size)
           (%deliver-darwin-memory-sample
            sampled-at statistics memory-total page-size collector
            on-sample on-unavailable)
           (funcall on-unavailable :read-failed))))
   on-unavailable))

#+darwin
(defun %read-darwin-memory-total
    (sampled-at statistics collector on-sample on-unavailable)
  (ntop.platform.darwin:sysctl-read-by-name
   "hw.memsize"
   (lambda (octets)
     (let ((memory-total (%decode-darwin-sysctl-integer octets)))
       (if (%memory-counter-p memory-total)
           (%read-darwin-memory-page-size
            sampled-at statistics memory-total collector on-sample on-unavailable)
           (funcall on-unavailable :read-failed))))
   on-unavailable))

#+darwin
(defun %collect-darwin-memory-sample
    (collector sampled-at on-sample on-unavailable)
  (ntop.platform.darwin:mach-read-host-vm-statistics64
   (lambda (statistics)
     (%read-darwin-memory-total
      sampled-at statistics collector on-sample on-unavailable))
   on-unavailable))

(defun %linux-meminfo-bytes (meminfo key)
  (let ((entry (assoc key meminfo :test #'string=)))
    (when (and entry
               (%memory-counter-p (second entry))
               (stringp (third entry))
               (string= (third entry) "kB"))
      (* (second entry) 1024))))

(defun %linux-cgroup-inactive-file-bytes (cgroup)
  (let* ((key (if (= (getf cgroup :version) 1)
                  "total_inactive_file"
                  "inactive_file"))
         (entry (assoc key (getf cgroup :memory-stat) :test #'string=)))
    (and entry (%memory-counter-p (cdr entry)) (cdr entry))))

(defun %linux-cgroup-memory-values (cgroup physical-total)
  (let ((usage (getf cgroup :memory-usage)))
    (when (and (%memory-counter-p usage)
               (%memory-counter-p physical-total))
      (let* ((inactive (%linux-cgroup-inactive-file-bytes cgroup))
             (limit (getf cgroup :memory-limit))
             (used (if (and inactive (< inactive usage))
                       (- usage inactive)
                       usage))
             (total (if (%memory-counter-p limit)
                        (min limit physical-total)
                        physical-total)))
        (values used total)))))

(defun %linux-memory-sample-from-data
    (sampled-at meminfo cgroup enable-cache-memory-p)
  (let* ((physical-total (%linux-meminfo-bytes meminfo "MemTotal"))
         (available (%linux-meminfo-bytes meminfo "MemAvailable"))
         (free (%linux-meminfo-bytes meminfo "MemFree"))
         (swap-total (%linux-meminfo-bytes meminfo "SwapTotal"))
         (swap-free (%linux-meminfo-bytes meminfo "SwapFree")))
    (multiple-value-bind (cgroup-used cgroup-total)
        (if cgroup
            (%linux-cgroup-memory-values cgroup physical-total)
            (values nil nil))
      (let* ((ram-used (or cgroup-used
                           (when (and physical-total available
                                      (<= available physical-total))
                             (- physical-total available))))
             (ram-total (or cgroup-total physical-total))
             (swap-used (when (and swap-total swap-free
                                   (<= swap-free swap-total))
                          (- swap-total swap-free)))
             (cache-used (when (and enable-cache-memory-p available free)
                           (let ((value
                                   (ntop.memory.domain:memory-cache-used-bytes
                                    available free)))
                             (and (%memory-counter-p value) value)))))
        (when (or ram-total swap-total)
          (ntop.memory.domain:make-memory-sample
           :sampled-at sampled-at
           :ram-used-bytes ram-used
           :ram-total-bytes ram-total
           :swap-used-bytes swap-used
           :swap-total-bytes swap-total
           :cache-used-bytes cache-used
           :cache-total-bytes (and enable-cache-memory-p physical-total)))))))

(defun %collect-linux-memory-sample
    (collector sampled-at on-sample on-unavailable)
  (ntop.platform.linux:read-linux-proc-meminfo
   (lambda (meminfo)
     (ntop.platform.linux:read-linux-cgroup-data
      (lambda (cgroup)
        (let ((sample
                (%linux-memory-sample-from-data
                 sampled-at meminfo cgroup
                 (memory-collector-enable-cache-memory-p collector))))
          (if sample
              (funcall on-sample sample)
              (funcall on-unavailable :read-failed))))
      (lambda (reason)
        (declare (ignore reason))
        (let ((sample
                (%linux-memory-sample-from-data
                 sampled-at meminfo nil
                 (memory-collector-enable-cache-memory-p collector))))
          (if sample
              (funcall on-sample sample)
              (funcall on-unavailable :read-failed))))))
   on-unavailable))

(defun %memory-unavailable-callback (on-unavailable)
  (lambda (reason)
    (funcall on-unavailable
             (if (typep reason
                        'ntop.shared.application:collector-unavailability-reason)
                 reason
                 :read-failed))))

(defmethod ntop.shared.application:collect-memory-sample
    ((collector memory-collector) on-sample on-unavailable)
  (let ((memory-on-unavailable
          (%memory-unavailable-callback on-unavailable)))
    (ntop.shared.domain:read-monotonic-time
     (memory-collector-clock collector)
     (lambda (sampled-at)
       #+darwin
       (%collect-darwin-memory-sample
        collector sampled-at on-sample memory-on-unavailable)
       #+linux
       (%collect-linux-memory-sample
        collector sampled-at on-sample memory-on-unavailable)
       #-(or darwin linux)
       (funcall memory-on-unavailable :unsupported-platform))
     memory-on-unavailable)))

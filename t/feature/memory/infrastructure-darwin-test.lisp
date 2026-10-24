(in-package #:ntop.test)

(defparameter +darwin-memory-reference-fields+
  '((:free-count "Pages free")
    (:active-count "Pages active")
    (:inactive-count "Pages inactive")
    (:wire-count "Pages wired down")
    (:purgeable-count "Pages purgeable")
    (:speculative-count "Pages speculative")
    (:compressor-page-count "Pages occupied by compressor")
    (:internal-page-count "Anonymous pages")))

(defun %darwin-memory-reference-command-output (program arguments)
  (let ((result (host-kit:run-program program arguments
                                      :timeout 5
                                      :max-output-characters 4096)))
    (unless (and (zerop (host-kit:process-result-exit-code result))
                 (not (host-kit:process-result-stdout-truncated-p result))
                 (not (host-kit:process-result-stderr-truncated-p result)))
      (error "Reference command failed or exceeded its output limit: ~A."
             program))
    (host-kit:process-result-stdout result)))

(defun %darwin-memory-reference-vm-stat ()
  (let* ((output
           (%darwin-memory-reference-command-output "vm_stat" nil))
         (marker (search "page size of " output)))
    (unless marker
      (error "vm_stat did not report its page size."))
    (let* ((page-size-start (+ marker (length "page size of ")))
           (page-size-end (position #\Space output :start page-size-start))
           (counts
             (loop for line in
                   (with-input-from-string (stream output)
                     (loop for item = (read-line stream nil)
                           while item collect item))
                   for separator = (position #\: line)
                   when separator
                     append
                     (let* ((label
                              (string-trim '(#\Space #\Tab #\Return #\Newline)
                                           (subseq line 0 separator)))
                            (field
                              (find label +darwin-memory-reference-fields+
                                    :key #'second :test #'string=)))
                       (when field
                         (let* ((text
                                  (string-right-trim
                                   '(#\.)
                                   (string-trim
                                    '(#\Space #\Tab #\Return #\Newline)
                                    (subseq line (1+ separator)))))
                                (value (parse-integer text)))
                           (list (first field) value)))))))
      (unless (and page-size-end
                   (= (length counts)
                      (* 2 (length +darwin-memory-reference-fields+))))
        (error "vm_stat did not report all memory page counts."))
      (list :page-size
            (parse-integer output :start page-size-start :end page-size-end)
            :counts counts))))

(defun %darwin-memory-reference-sysctl (name)
  (parse-integer
   (string-trim '(#\Space #\Tab #\Return #\Newline)
                (%darwin-memory-reference-command-output
                 "sysctl" (list "-n" name)))))

(defun %darwin-memory-total-pages (memory-bytes page-size)
  (unless (and (plusp page-size) (zerop (mod memory-bytes page-size)))
    (error "Memory size is not a positive whole number of pages."))
  (/ memory-bytes page-size))

(defun %darwin-memory-used-bytes (page-counts page-size)
  (* (+ (- (getf page-counts :internal-page-count)
           (getf page-counts :purgeable-count))
        (getf page-counts :wire-count)
        (getf page-counts :compressor-page-count))
     page-size))

(defun %darwin-memory-page-count-within-range-p
    (value before after tolerance)
  (and (integerp value)
       (<= (- (min before after) tolerance)
           value
           (+ (max before after) tolerance))))

(defun %collect-live-darwin-memory-sample ()
  (let ((result nil))
    (ntop.shared.application:collect-memory-sample
     (ntop.memory.infrastructure:make-memory-collector
      :clock (make-instance '%test-fixture-clock :time 42.25))
     (lambda (sample) (setf result (list :sample sample)))
     (lambda (reason) (setf result (list :unavailable reason))))
    result))

(cl-weave:describe "Darwin memory boundary"
  (cl-weave:it-run-if
   (member :darwin *features*)
   "matches bounded vm_stat page counts around the Mach VM read"
   (let* ((memory-before (%darwin-memory-reference-sysctl "hw.memsize"))
          (page-size-before (%darwin-memory-reference-sysctl "hw.pagesize"))
          (before (%darwin-memory-reference-vm-stat))
          (octets nil)
          (failure nil))
     (ntop.platform.darwin:mach-read-host-vm-statistics64
      (lambda (value) (setf octets value))
      (lambda (reason) (setf failure reason)))
     (let* ((after (%darwin-memory-reference-vm-stat))
            (page-size-after
              (%darwin-memory-reference-sysctl "hw.pagesize"))
            (memory-after (%darwin-memory-reference-sysctl "hw.memsize"))
            (page-counts
              (ntop.memory.infrastructure::decode-darwin-vm-statistics64 octets))
            (total-pages
              (%darwin-memory-total-pages memory-before page-size-before))
            (tolerance (max 1 (ceiling total-pages 100))))
       (cl-weave:expect failure :to-equal nil)
       (cl-weave:expect
        (list (getf before :page-size)
              page-size-before page-size-after
              (getf after :page-size)
              memory-after)
        :to-equal
        (list page-size-before page-size-before page-size-before
              page-size-before memory-before))
       (cl-weave:expect
        (loop for field in +darwin-memory-reference-fields+
              for key = (first field)
              for value = (getf page-counts key)
              for reference-before = (if (eq key :free-count)
                                          (+ (getf (getf before :counts) :free-count)
                                             (getf (getf before :counts) :speculative-count))
                                          (getf (getf before :counts) key))
              for reference-after = (if (eq key :free-count)
                                         (+ (getf (getf after :counts) :free-count)
                                            (getf (getf after :counts) :speculative-count))
                                         (getf (getf after :counts) key))
              unless (%darwin-memory-page-count-within-range-p
                      value reference-before reference-after tolerance)
                collect (list key value reference-before reference-after))
        :to-equal nil))))

  (cl-weave:it-run-if
   (member :darwin *features*)
   "collects memory values through the Darwin Mach and sysctl APIs"
   (let* ((memory-before (%darwin-memory-reference-sysctl "hw.memsize"))
          (page-size-before (%darwin-memory-reference-sysctl "hw.pagesize"))
          (before (%darwin-memory-reference-vm-stat))
          (result (%collect-live-darwin-memory-sample))
          (after (%darwin-memory-reference-vm-stat))
          (page-size-after (%darwin-memory-reference-sysctl "hw.pagesize"))
          (memory-after (%darwin-memory-reference-sysctl "hw.memsize"))
          (total-pages
            (%darwin-memory-total-pages memory-before page-size-before))
          (tolerance-bytes (* (max 1 (ceiling total-pages 100))
                              page-size-before))
          (sample (second result)))
     (cl-weave:expect (first result) :to-be :sample)
     (cl-weave:expect
      (list page-size-before page-size-after
            (getf before :page-size) (getf after :page-size)
            memory-after
            (ntop.shared.domain:sample-record-sampled-at sample)
            (ntop.memory.domain:memory-sample-ram-total-bytes sample))
      :to-equal
      (list page-size-before page-size-before page-size-before page-size-before
            memory-before 42.25 memory-before))
     (cl-weave:expect
      (%darwin-memory-page-count-within-range-p
       (ntop.memory.domain:memory-sample-ram-used-bytes sample)
       (%darwin-memory-used-bytes (getf before :counts) page-size-before)
       (%darwin-memory-used-bytes (getf after :counts) page-size-before)
       tolerance-bytes)
      :to-be t)
     (cl-weave:expect
      (let ((used (ntop.memory.domain:memory-sample-swap-used-bytes sample))
            (total (ntop.memory.domain:memory-sample-swap-total-bytes sample)))
        (and (integerp used) (integerp total) (<= 0 used total)))
      :to-be t))))

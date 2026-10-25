(in-package #:ntop.test)

(defun %ioaccel-cf-dictionary-tree (entries)
  (list :dictionary
        (mapcar
         (lambda (entry)
           (cons (list :string (car entry)) (cdr entry)))
         entries)))

(cl-weave:describe "IOAccelerator property fixtures"
                   (cl-weave:it
                    "converts PerformanceStatistics values to a GPU row"
                    (let* ((tree
                            (%ioaccel-cf-dictionary-tree
                             `(("IOClass" . (:string "AGXAcceleratorG16X"))
                               ("PerformanceStatistics" .
                                                        ,(%ioaccel-cf-dictionary-tree
                                                          '(("Device Utilization %"
                                                             .
                                                             (:number 33))
                                                            ("In use system memory"
                                                             .
                                                             (:number
                                                              2216853504))
                                                            ("Alloc system memory"
                                                             .
                                                             (:number
                                                              3191881728))))))))
                           (properties
                            (ntop.platform.darwin::cf-tree-to-lisp tree)))
                      (cl-weave:expect
                       (ntop.platform.darwin::decode-darwin-ioaccel-device-properties
                        properties)
                       :to-equal
                       '(:name "AGXAcceleratorG16X"
                               :usage-percent
                               33
                               :memory-used-bytes
                               2216853504
                               :memory-total-bytes
                               3191881728
                               :temperature-celsius
                               nil))))
                   (cl-weave:it
                    "preserves a temperature when the statistics key exists"
                    (let* ((tree
                            (%ioaccel-cf-dictionary-tree
                             `(("IOClass" . (:string "FixtureAccelerator"))
                               ("PerformanceStatistics" .
                                                        ,(%ioaccel-cf-dictionary-tree
                                                          '(("Device Utilization %"
                                                             .
                                                             (:number 0.0d0))
                                                            ("In use system memory"
                                                             .
                                                             (:number 0))
                                                            ("Alloc system memory"
                                                             .
                                                             (:number 0))
                                                            ("Temperature" .
                                                                           (:number
                                                                            47.5d0))))))))
                           (properties
                            (ntop.platform.darwin::cf-tree-to-lisp tree)))
                      (cl-weave:expect
                       (ntop.platform.darwin::decode-darwin-ioaccel-device-properties
                        properties)
                       :to-equal
                       '(:name "FixtureAccelerator"
                               :usage-percent
                               0.0d0
                               :memory-used-bytes
                               0
                               :memory-total-bytes
                               0
                               :temperature-celsius
                               47.5d0))))
                   (cl-weave:it
                    "rejects a non-dictionary PerformanceStatistics value"
                    (let ((properties (make-hash-table :test #'equal)))
                      (setf (gethash "PerformanceStatistics" properties) nil)
                      (cl-weave:expect
                       (lambda ()
                         (ntop.platform.darwin::decode-darwin-ioaccel-device-properties
                          properties))
                       :to-throw
                       'error))))

(cl-weave:describe "Darwin GPU collection outcomes"
                   (cl-weave:it
                    "maps IOAccelerator failures to the shared reason keywords"
                    (dolist
                        (case '((nil :not-present)
                                ((:permission-denied) :permission-denied)
                                (((:iokit-error #xe00002c1)) :permission-denied)
                                (((:iokit-error #xe00002c0)) :not-present)
                                (((:iokit-error #xe00002c2)) :read-failed)
                                (((:iokit-error "invalid")) :read-failed)
                                ((:null-properties) :read-failed)
                                ((:null-properties (:iokit-error #xe00002c1))
                                 :permission-denied)))
                      (destructuring-bind (failures expected) case
                        (cl-weave:expect
                         (ntop.platform.darwin::%darwin-gpu-unavailable-reason
                          failures)
                         :to-be
                         expected))))
                   (cl-weave:it-run-if
                    (member :darwin *features*)
                    "returns decoded GPU rows and unavailable reasons from fake IOAccelerator ports"
                    (let ((mode :success)
                          (invalid-properties (make-hash-table :test #'equal))
                          (properties (make-hash-table :test #'equal))
                          (statistics (make-hash-table :test #'equal))
                          (calls nil))
                      (setf (gethash "PerformanceStatistics" invalid-properties) nil
                            (gethash "IOClass" properties) "FixtureAccelerator"
                            (gethash "PerformanceStatistics" properties) statistics
                            (gethash "Device Utilization %" statistics) 42
                            (gethash "In use system memory" statistics) 1024
                            (gethash "Alloc system memory" statistics) 2048
                            (gethash "Temperature" statistics) 55.5d0)
                      (cl-weave:with-mocked-functions
                       (((symbol-function
                          'ntop.platform.darwin:call-with-iokit-services)
                         (lambda (service-class on-service on-complete on-error)
                           (push (list :services service-class) calls)
                           (case mode
                             (:service-status
                              (funcall on-error '(:iokit-error #xe00002c0)))
                             (:service-throws
                              (error "IOKit service lookup failed."))
                             (otherwise
                              (funcall on-service 41)
                              (funcall on-complete)))))
                        ((symbol-function
                          'ntop.platform.darwin:iokit-registry-properties)
                         (lambda (service on-value on-error)
                           (push (list :properties service) calls)
                           (ecase mode
                             (:success
                              (funcall on-value properties))
                             (:properties-status
                              (funcall on-error '(:iokit-error #xe00002c1)))
                             (:invalid-properties
                              (funcall on-value invalid-properties))))))
                       (flet ((read-result ()
                                (let ((outcome nil))
                                  (ntop.platform.darwin::darwin-read-gpu-device-data
                                   (lambda (rows)
                                     (setf outcome (list :value rows)))
                                   (lambda (reason)
                                     (setf outcome
                                           (list :unavailable reason))))
                                  outcome)))
                         (cl-weave:expect
                          (read-result)
                          :to-equal
                          '(:value
                            ((:name "FixtureAccelerator"
                                    :usage-percent 42
                                    :memory-used-bytes 1024
                                    :memory-total-bytes 2048
                                    :temperature-celsius 55.5d0))))
                         (cl-weave:expect
                          (reverse calls)
                          :to-equal
                          '((:services "IOAccelerator") (:properties 41)))
                         (setf mode :properties-status
                               calls nil)
                         (cl-weave:expect
                          (read-result)
                          :to-equal
                          '(:unavailable :permission-denied))
                         (cl-weave:expect
                          (reverse calls)
                          :to-equal
                          '((:services "IOAccelerator") (:properties 41)))
                         (setf mode :invalid-properties
                               calls nil)
                         (cl-weave:expect
                          (read-result)
                          :to-equal
                          '(:unavailable :read-failed))
                         (cl-weave:expect
                          (reverse calls)
                          :to-equal
                          '((:services "IOAccelerator") (:properties 41)))
                         (setf mode :service-status
                               calls nil)
                         (cl-weave:expect
                          (read-result)
                          :to-equal
                          '(:unavailable :not-present))
                         (cl-weave:expect
                          (reverse calls)
                          :to-equal
                          '((:services "IOAccelerator")))
                         (setf mode :service-throws
                               calls nil)
                         (cl-weave:expect
                          (read-result)
                          :to-equal
                          '(:unavailable :read-failed))
                         (cl-weave:expect
                          (reverse calls)
                          :to-equal
                          '((:services "IOAccelerator"))))))))

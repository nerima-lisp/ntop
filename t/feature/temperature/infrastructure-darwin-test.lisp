(in-package #:ntop.test)

(defclass %darwin-temperature-cpu-clock ()
  ())

(defmethod ntop.shared.domain:read-monotonic-time ((clock
                                                    %darwin-temperature-cpu-clock)
                                                   on-time
                                                   on-unavailable)
  (declare (ignore clock on-unavailable))
  (funcall on-time 1.0d0))

(defun %collect-darwin-temperature-sample (collector)
  (let ((result nil))
    (ntop.shared.application:collect-temperature-sample collector
                                                        (lambda (sample)
                                                          (setf result (list
                                                                        :sample
                                                                        sample)))
                                                        (lambda (reason)
                                                          (setf result (list
                                                                        :unavailable
                                                                        reason))))
    result))

(defun %darwin-temperature-rows (sample)
  (mapcar
   (lambda (sensor)
     (list (ntop.temperature.domain:temperature-sensor-name sensor)
           (ntop.temperature.domain:temperature-sensor-temperature-celsius
            sensor)))
   (ntop.temperature.domain:temperature-sample-sensors sample)))

(cl-weave:describe "Darwin temperature collector"
                   (cl-weave:it
                    "keeps application collection available when IOHID loading fails"
                    (let* ((loader-calls 0)
                           (collector
                            (ntop.temperature.infrastructure:make-darwin-temperature-collector
                             :api-loader
                             (lambda ()
                               (incf loader-calls)
                               (error "Injected IOHID loader failure."))
                             :now
                             (lambda ()
                               0.0d0)))
                           (clock
                            (make-instance '%darwin-temperature-cpu-clock))
                           (cpu-collector
                            (ntop.cpu.infrastructure:make-cpu-collector
                             :source-reader
                             (lambda (on-value on-unavailable)
                               (declare (ignore on-unavailable))
                               (funcall on-value
                                        (ntop.cpu.infrastructure::make-cpu-source-sample
                                         :core-values
                                         (list
                                          (ntop.cpu.domain:make-cpu-core-value
                                           :core-index 0)))))
                             :clock
                             clock))
                           (snapshot nil)
                           (top-level-unavailable :not-called))
                      (progn
                        (cl-weave:expect loader-calls :to-be 0)
                        (cl-weave:expect
                         (%collect-darwin-temperature-sample collector)
                         :to-equal
                         (quote (:unavailable :private-api-unavailable))))
                      (ntop.app.application:collect-application-snapshot
                       (list :cpu cpu-collector :temperature collector)
                       clock
                       (lambda (value)
                         (setf snapshot value))
                       (lambda (reason)
                         (setf top-level-unavailable reason)))
                      (cl-weave:expect loader-calls :to-be 2)
                      (cl-weave:expect
                       (ntop.app.domain:collection-snapshot-p snapshot)
                       :to-be
                       t)
                      (cl-weave:expect
                       (ntop.app.domain:collection-snapshot-temperature-sample
                        snapshot)
                       :to-be
                       nil)
                      (cl-weave:expect
                       (let* ((cpu-sample
                               (ntop.app.domain:collection-snapshot-cpu-sample
                                snapshot))
                              (cpu-row
                               (find :cpu
                                     (ntop.cpu.presentation::%cpu-legend-rows
                                      cpu-sample
                                      (ntop.shared.application:make-application-config)
                                      5)
                                     :key
                                     (lambda (row) (getf row :kind)))))
                         (list
                          (not (null cpu-sample))
                          (ntop.cpu.domain:cpu-core-value-frequency-mhz
                           (first
                            (ntop.cpu.domain:cpu-sample-core-values cpu-sample)))
                          (getf cpu-row :frequency)))
                       :to-equal
                       (quote (t nil "N/A")))
                      (cl-weave:expect top-level-unavailable :to-be :not-called)))
                   (cl-weave:it
                    "keeps duplicate named services as rows and samples cached handles again"
                    (let* ((times '(0.0d0 1.0d0))
                           (loader-calls 0)
                           (enumeration-count 0)
                           (read-count 0)
                           (collector
                            (ntop.temperature.infrastructure:make-darwin-temperature-collector
                             :api-loader
                             (lambda ()
                               (incf loader-calls)
                               (list :enumerate-services
                                     (lambda ()
                                       (incf enumeration-count)
                                       '((101 "Sensor" :service-a)
                                         (102 "Sensor" :service-b)
                                         (103 "Package" :service-c)
                                         (104 nil :unnamed-service)))
                                     :read-temperature
                                     (lambda (handle)
                                       (incf read-count)
                                       (ecase handle
                                         (:service-a 40.0d0)
                                         (:service-b 41.0d0)
                                         (:service-c nil)))))
                             :now
                             (lambda ()
                               (pop times))))
                           (first-result nil)
                           (second-result nil))
                      (cl-weave:expect loader-calls :to-be 0)
                      (setf first-result (%collect-darwin-temperature-sample
                                          collector)
                            second-result (%collect-darwin-temperature-sample
                                           collector))
                      (cl-weave:expect loader-calls :to-be 1)
                      (cl-weave:expect enumeration-count :to-be 1)
                      (cl-weave:expect read-count :to-be 6)
                      (cl-weave:expect (first first-result) :to-be :sample)
                      (cl-weave:expect
                       (%darwin-temperature-rows (second first-result))
                       :to-equal
                       '(("Sensor" 40.0d0) ("Sensor" 41.0d0) ("Package" nil)))
                      (cl-weave:expect
                       (%darwin-temperature-rows (second second-result))
                       :to-equal
                       '(("Sensor" 40.0d0) ("Sensor" 41.0d0) ("Package" nil)))))
                   (cl-weave:it
                    "reports absent sensors with the shared unavailable reason"
                    (let* ((enumeration-count 0)
                           (read-count 0)
                           (collector
                            (ntop.temperature.infrastructure:make-darwin-temperature-collector
                             :api-loader
                             (lambda ()
                               (list :enumerate-services
                                     (lambda ()
                                       (incf enumeration-count)
                                       nil)
                                     :read-temperature
                                     (lambda (handle)
                                       (declare (ignore handle))
                                       (incf read-count))))
                             :now
                             (lambda ()
                               0.0d0))))
                      (cl-weave:expect
                       (%collect-darwin-temperature-sample collector)
                       :to-equal
                       (quote (:unavailable :not-present)))
                      (cl-weave:expect enumeration-count :to-be 1)
                      (cl-weave:expect read-count :to-be 0)))
                   (cl-weave:it
                    "validates API loaders and maps enumeration and read exceptions"
                    (cl-weave:signals
                     simple-error
                     (ntop.temperature.infrastructure:make-darwin-temperature-collector
                      :api-loader nil))
                    (let ((collector
                           (ntop.temperature.infrastructure:make-darwin-temperature-collector
                            :api-loader (lambda ()
                                          (list :enumerate-services nil
                                                :read-temperature (lambda (handle)
                                                                    (declare (ignore handle)))))
                            :now (lambda () 0.0d0))))
                      (cl-weave:expect
                       (%collect-darwin-temperature-sample collector)
                       :to-equal
                       '(:unavailable :private-api-unavailable)))
                    (let ((collector
                           (ntop.temperature.infrastructure:make-darwin-temperature-collector
                            :api-loader (lambda ()
                                          (list :enumerate-services
                                                (lambda () (error "Injected enumeration failure."))
                                                :read-temperature (lambda (handle)
                                                                    (declare (ignore handle)))))
                            :now (lambda () 0.0d0))))
                      (cl-weave:expect
                       (%collect-darwin-temperature-sample collector)
                       :to-equal
                       '(:unavailable :read-failed)))
                    (let* ((collector
                            (ntop.temperature.infrastructure:make-darwin-temperature-collector
                             :api-loader (lambda ()
                                           (list :enumerate-services
                                                 (lambda () '((101 "CPU" :cpu)))
                                                 :read-temperature (lambda (handle)
                                                                     (declare (ignore handle))
                                                                     (error
                                                                      "Injected read failure."))))
                             :now (lambda () 0.0d0)))
                           (result (%collect-darwin-temperature-sample collector)))
                      (cl-weave:expect (first result) :to-be :sample)
                      (cl-weave:expect (%darwin-temperature-rows (second result))
                                       :to-equal
                                       '(("CPU" nil))))))


(in-package #:ntop.test)

(defun %temperature-test-collect (collector)
  (let ((result nil))
    (ntop.shared.application:collect-temperature-sample collector
                                                        (lambda (sample)
                                                          (setf result (list
                                                                        :value
                                                                        sample)))
                                                        (lambda (reason)
                                                          (setf result (list
                                                                        :unavailable
                                                                        reason))))
    result))

(defun %temperature-test-sensors (result)
  (ntop.temperature.domain:temperature-sample-sensors (second result)))

(cl-weave:describe "temperature infrastructure collector"
                   (cl-weave:it
                    "refreshes its source cache at sixty seconds and creates fresh samples"
                    (let* ((now 0)
                           (enumeration-count 0)
                           (sources
                            (list
                             (ntop.temperature.infrastructure:make-temperature-source
                              :identity
                              "cpu0"
                              :name
                              "CPU"
                              :handle
                              :cpu0)))
                           (collector
                            (ntop.temperature.infrastructure:make-temperature-collector
                             :enumerate-sensors
                             (lambda (on-sources on-unavailable)
                               (declare (ignore on-unavailable))
                               (incf enumeration-count)
                               (funcall on-sources (copy-list sources)))
                             :read-sensors
                             (lambda (read-sources on-values on-unavailable)
                               (declare (ignore on-unavailable))
                               (funcall on-values
                                        (mapcar
                                         (lambda (source)
                                           (cons
                                            (ntop.temperature.infrastructure:temperature-source-identity
                                             source)
                                            45))
                                         read-sources)))
                             :now
                             (lambda ()
                               now)))
                           (first-result (%temperature-test-collect collector))
                           (first-sample (second first-result)))
                      (setf sources (list
                                     (ntop.temperature.infrastructure:make-temperature-source
                                      :identity
                                      "gpu0"
                                      :name
                                      "GPU"
                                      :handle
                                      :gpu0))
                            now 59)
                      (let* ((before-expiry
                              (%temperature-test-collect collector))
                             (before-expiry-sample (second before-expiry)))
                        (setf now 60)
                        (let* ((at-expiry (%temperature-test-collect collector))
                               (at-expiry-sample (second at-expiry)))
                          (cl-weave:expect enumeration-count :to-be 2)
                          (cl-weave:expect
                           (mapcar
                            #'ntop.temperature.domain:temperature-sensor-name
                            (%temperature-test-sensors first-result))
                           :to-equal
                           '("CPU"))
                          (cl-weave:expect
                           (mapcar
                            #'ntop.temperature.domain:temperature-sensor-name
                            (%temperature-test-sensors before-expiry))
                           :to-equal
                           '("CPU"))
                          (cl-weave:expect
                           (mapcar
                            #'ntop.temperature.domain:temperature-sensor-name
                            (%temperature-test-sensors at-expiry))
                           :to-equal
                           '("GPU"))
                          (cl-weave:expect
                           (ntop.shared.domain:sample-record-sampled-at
                            first-sample)
                           :to-be
                           0)
                          (cl-weave:expect
                           (ntop.shared.domain:sample-record-sampled-at
                            before-expiry-sample)
                           :to-be
                           59)
                          (cl-weave:expect
                           (ntop.shared.domain:sample-record-sampled-at
                            at-expiry-sample)
                           :to-be
                           60)
                          (cl-weave:expect
                           (not (eq first-sample before-expiry-sample))
                           :to-be
                           t)))))
                   (cl-weave:it
                    "matches identities with equal and keeps missing readings as N/A rows"
                    (let* ((sources
                            (list
                             (ntop.temperature.infrastructure:make-temperature-source
                              :identity
                              "cpu0"
                              :name
                              "CPU 0"
                              :handle
                              :cpu0)
                             (ntop.temperature.infrastructure:make-temperature-source
                              :identity
                              "cpu1"
                              :name
                              "CPU 1"
                              :handle
                              :cpu1)
                             (ntop.temperature.infrastructure:make-temperature-source
                              :identity
                              "cpu2"
                              :name
                              "CPU 2"
                              :handle
                              :cpu2)))
                           (collector
                            (ntop.temperature.infrastructure:make-temperature-collector
                             :enumerate-sensors
                             (lambda (on-sources on-unavailable)
                               (declare (ignore on-unavailable))
                               (funcall on-sources sources))
                             :read-sensors
                             (lambda (read-sources on-values on-unavailable)
                               (declare (ignore read-sources on-unavailable))
                               (funcall on-values
                                        (list (cons (copy-seq "cpu0") 42.5)
                                              (cons "cpu1" nil))))
                             :now
                             (lambda ()
                               12)))
                           (result (%temperature-test-collect collector))
                           (sample (second result))
                           (sensors (%temperature-test-sensors result)))
                      (cl-weave:expect (first result) :to-be :value)
                      (cl-weave:expect (length sensors) :to-be 3)
                      (cl-weave:expect
                       (mapcar
                        #'ntop.temperature.domain:temperature-sensor-temperature-celsius
                        sensors)
                       :to-equal
                       '(42.5 nil nil))
                      (cl-weave:expect
                       (handler-case (progn
                                       (funcall
                                        (fdefinition
                                         '(setf ntop.temperature.domain:temperature-sample-sensors))
                                        nil
                                        sample)
                                       nil)
                         (undefined-function ()
                           t))
                       :to-be
                       t)))
                   (cl-weave:it
                    "forwards enumeration failures without reading or emitting a sample"
                    (let* ((reader-called nil) (sample nil) (unavailable nil) (collector (ntop.temperature.infrastructure:make-temperature-collector :enumerate-sensors (lambda (on-sources on-unavailable) (declare (ignore on-sources)) (funcall on-unavailable :read-failed)) :read-sensors (lambda (sources on-values on-unavailable) (declare (ignore sources on-values on-unavailable)) (setf reader-called t)) :now (lambda () 0)))) (ntop.shared.application:collect-temperature-sample collector (lambda (value) (setf sample value)) (lambda (reason) (setf unavailable reason))) (cl-weave:expect reader-called :to-be nil) (cl-weave:expect sample :to-be nil) (cl-weave:expect unavailable :to-be :read-failed))))

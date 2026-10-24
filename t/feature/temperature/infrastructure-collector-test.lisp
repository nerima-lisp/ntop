(in-package #:ntop.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load
   (merge-pathnames
    "helpers-infrastructure.lisp"
    (uiop:pathname-directory-pathname
     (asdf:component-pathname
      (asdf:find-component
       "ntop/test"
       "t/feature/temperature/infrastructure-collector-test"))))))

(cl-weave:describe
 "temperature infrastructure collector"
 (cl-weave:it
  "refreshes its source cache at sixty seconds and creates fresh samples"
  (let* ((now 0)
         (enumeration-count 0)
         (sources
          (list
           (ntop.temperature.infrastructure:make-temperature-source :identity
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
   (let* ((before-expiry (%temperature-test-collect collector))
          (before-expiry-sample (second before-expiry)))
    (setf now 60)
    (let* ((at-expiry (%temperature-test-collect collector))
           (at-expiry-sample (second at-expiry)))
     (cl-weave:expect
      (list enumeration-count
            (mapcar (lambda (sensor)
                      (ntop.temperature.domain:temperature-sensor-name sensor))
                    (%temperature-test-sensors first-result))
            (mapcar (lambda (sensor)
                      (ntop.temperature.domain:temperature-sensor-name sensor))
                    (%temperature-test-sensors before-expiry))
            (mapcar (lambda (sensor)
                      (ntop.temperature.domain:temperature-sensor-name sensor))
                    (%temperature-test-sensors at-expiry))
            (ntop.shared.domain:sample-record-sampled-at first-sample)
            (ntop.shared.domain:sample-record-sampled-at before-expiry-sample)
            (ntop.shared.domain:sample-record-sampled-at at-expiry-sample)
            (not (eq first-sample before-expiry-sample)))
      :to-equal
      (list 2 (list "CPU") (list "CPU") (list "GPU") 0 59 60 t))))))
 (cl-weave:it
  "matches identities with equal and keeps missing readings as N/A rows"
  (let* ((sources
          (list
           (ntop.temperature.infrastructure:make-temperature-source :identity
                                                                    "cpu0"
                                                                    :name
                                                                    "CPU 0"
                                                                    :handle
                                                                    :cpu0)
           (ntop.temperature.infrastructure:make-temperature-source :identity
                                                                    "cpu1"
                                                                    :name
                                                                    "CPU 1"
                                                                    :handle
                                                                    :cpu1)
           (ntop.temperature.infrastructure:make-temperature-source :identity
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
                     (list (cons (copy-seq "cpu0") 42.5) (cons "cpu1" nil))))
           :now
           (lambda ()
            12)))
         (result (%temperature-test-collect collector))
         (sample (second result))
         (sensors (%temperature-test-sensors result)))
   (cl-weave:expect
    (list (first result)
          (length sensors)
          (mapcar (lambda (sensor)
                    (ntop.temperature.domain:temperature-sensor-temperature-celsius
                     sensor))
                  sensors)
          (handler-case
              (progn
                (funcall
                 (fdefinition
                  (quote (setf ntop.temperature.domain:temperature-sample-sensors)))
                 nil
                 sample)
                nil)
            (undefined-function () t)))
    :to-equal
    (list :value 3 (list 42.5 nil nil) t))))
 (cl-weave:it
  "forwards enumeration failures without reading or emitting a sample"
  (let* ((reader-called nil)
         (sample nil)
         (unavailable nil)
         (collector
          (ntop.temperature.infrastructure:make-temperature-collector
           :enumerate-sensors
           (lambda (on-sources on-unavailable)
            (declare (ignore on-sources))
            (funcall on-unavailable :read-failed))
           :read-sensors
           (lambda (sources on-values on-unavailable)
            (declare (ignore sources on-values on-unavailable))
            (setf reader-called t))
           :now
           (lambda ()
            0))))
   (ntop.shared.application:collect-temperature-sample collector
                                                       (lambda (value)
                                                        (setf sample value))
                                                       (lambda (reason)
                                                        (setf unavailable reason)))
   (cl-weave:expect reader-called :to-be nil)
   (cl-weave:expect sample :to-be nil)
   (cl-weave:expect unavailable :to-be :read-failed)))
 (cl-weave:it "validates collector callbacks and forwards reader failures"
              (cl-weave:signals simple-error
                                (ntop.temperature.infrastructure:make-temperature-collector
                                 :read-sensors
                                 (lambda (sources on-values on-unavailable)
                                  (declare (ignore sources
                                                   on-values
                                                   on-unavailable)))))
              (cl-weave:signals simple-error
                                (ntop.temperature.infrastructure:make-temperature-collector
                                 :enumerate-sensors
                                 (lambda (on-sources on-unavailable)
                                  (declare (ignore on-sources on-unavailable)))))
              (cl-weave:signals simple-error
                                (ntop.temperature.infrastructure:make-temperature-collector
                                 :enumerate-sensors
                                 (lambda (on-sources on-unavailable)
                                  (declare (ignore on-sources on-unavailable)))
                                 :read-sensors
                                 (lambda (sources on-values on-unavailable)
                                  (declare (ignore sources
                                                   on-values
                                                   on-unavailable)))
                                 :now
                                 nil))
              (let* ((source
                      (ntop.temperature.infrastructure:make-temperature-source
                       :identity
                       "cpu0"
                       :name
                       "CPU"
                       :handle
                       :cpu0))
                     (collector
                      (ntop.temperature.infrastructure:make-temperature-collector
                       :enumerate-sensors
                       (lambda (on-sources on-unavailable)
                        (declare (ignore on-unavailable))
                        (funcall on-sources (list source)))
                       :read-sensors
                       (lambda (sources on-values on-unavailable)
                        (declare (ignore sources on-values))
                        (funcall on-unavailable :permission-denied))
                       :now
                       (lambda ()
                        0)))
                     (result (%temperature-test-collect collector)))
               (cl-weave:expect result
                                :to-equal
                                '(:unavailable :permission-denied))))
 (cl-weave:it
 "requires source identity and handle and validates display names"
 (cl-weave:signals simple-error
   (apply (symbol-function
           'ntop.temperature.infrastructure:make-temperature-source)
          (list :name "CPU" :handle :cpu0)))
 (cl-weave:signals simple-error
   (apply (symbol-function
           'ntop.temperature.infrastructure:make-temperature-source)
          (list :identity "cpu0" :name "CPU")))
 (cl-weave:signals type-error
   (apply
    (symbol-function
     'ntop.temperature.infrastructure:make-temperature-source)
    (list :identity "cpu0" :name 7 :handle :cpu0)))
 (cl-weave:expect
  (ntop.temperature.infrastructure:temperature-source-name
   (ntop.temperature.infrastructure:make-temperature-source
    :identity "cpu0" :name nil :handle :cpu0))
  :to-be nil))
 (cl-weave:it "uses deterministic monotonic clock readings"
             (let* ((clock-id nil)
                   (sampled-at nil)
                   (ntop.temperature.infrastructure::*temperature-clock-gettime*
                    (lambda (clock)
                      (setf clock-id clock)
                      (values 123 456789000)))
                   (collector
                    (ntop.temperature.infrastructure:make-temperature-collector
                     :enumerate-sensors
                     (lambda (on-sources on-unavailable)
                       (declare (ignore on-sources on-unavailable)))
                     :read-sensors
                     (lambda (sources on-values on-unavailable)
                       (declare (ignore sources on-values on-unavailable))))))
               (setf sampled-at (funcall
                                 (ntop.temperature.infrastructure::%temperature-now
                                  collector)))
               (cl-weave:expect (list clock-id sampled-at)
                                :to-equal
                                (list sb-unix:clock-monotonic 123.456789d0)))))


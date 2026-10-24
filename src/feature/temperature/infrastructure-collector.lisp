(in-package #:ntop.temperature.infrastructure)

 (defun %temperature-source-name-p (value)
  "Return true when VALUE is a supported temperature display name."
  (or (null value)
      (stringp value)))
 (defstruct (temperature-source
             (:copier nil)
             (:constructor %make-temperature-source))
  (identity (error "Temperature source identity is required.") :read-only t)
  (name nil :read-only t)
  (handle (error "Temperature source handle is required.") :read-only t))
 (defun make-temperature-source (&rest arguments)
  (let ((name (getf arguments :name)))
    (unless (%temperature-source-name-p name)
      (error (quote type-error)
             :datum name
             :expected-type (quote (or null string))))
    (apply (function %make-temperature-source) arguments)))

 (defconstant +temperature-source-cache-ttl-seconds+ 60)
 (defparameter *temperature-clock-gettime* (function sb-unix:clock-gettime))

 (defun %temperature-monotonic-seconds ()
   (multiple-value-bind (seconds nanoseconds)
       (funcall *temperature-clock-gettime* sb-unix:clock-monotonic)
     (+ seconds (/ nanoseconds 1000000000d0))))

 (defclass temperature-collector ()
   ((enumerate-sensors :initarg :enumerate-sensors
                       :initform nil
                       :reader %temperature-enumerate-sensors)
    (read-sensors :initarg :read-sensors
                  :initform nil
                  :reader %temperature-read-sensors)
    (now :initarg :now
         :initform #'%temperature-monotonic-seconds
         :reader %temperature-now)
    (source-cache :initform nil
                  :accessor %temperature-source-cache)
    (source-cache-present-p :initform nil
                            :accessor %temperature-source-cache-present-p)
    (source-cache-refreshed-at :initform nil
                               :accessor %temperature-source-cache-refreshed-at)))

 (defmethod initialize-instance :after
     ((collector temperature-collector) &key)
   (unless (functionp (%temperature-enumerate-sensors collector))
     (error "The temperature enumerator must be a function."))
   (unless (functionp (%temperature-read-sensors collector))
     (error "The temperature reader must be a function."))
   (unless (functionp (%temperature-now collector))
     (error "The temperature clock must be a function.")))

 (defun make-temperature-collector (&key enumerate-sensors read-sensors
                                      (now #'%temperature-monotonic-seconds))
   (make-instance 'temperature-collector
                  :enumerate-sensors enumerate-sensors
                  :read-sensors read-sensors
                  :now now))

 (defun %temperature-source-cache-expired-p (collector sampled-at)
   (or (not (%temperature-source-cache-present-p collector))
       (>= (- sampled-at
              (%temperature-source-cache-refreshed-at collector))
           +temperature-source-cache-ttl-seconds+)))

 (defun %temperature-sensors-from-values (sources values)
   (mapcar (lambda (source)
             (ntop.temperature.domain:make-temperature-sensor
              :name (temperature-source-name source)
              :temperature-celsius
              (cdr (assoc (temperature-source-identity source)
                          values
                          :test #'equal))))
           sources))

 (defun %collect-temperature-values
     (collector sources sampled-at on-sample on-unavailable)
   (funcall (%temperature-read-sensors collector)
            (copy-list sources)
            (lambda (values)
              (funcall on-sample
                       (ntop.temperature.domain:make-temperature-sample
                        :sampled-at sampled-at
                        :sensors
                        (%temperature-sensors-from-values sources values))))
            on-unavailable))

 (defmethod ntop.shared.application:collect-temperature-sample
     ((collector temperature-collector) on-sample on-unavailable)
   (let ((sampled-at (funcall (%temperature-now collector))))
     (if (%temperature-source-cache-expired-p collector sampled-at)
         (funcall (%temperature-enumerate-sensors collector)
                  (lambda (sources)
                    (setf (%temperature-source-cache collector)
                          (copy-list sources)
                          (%temperature-source-cache-present-p collector) t
                          (%temperature-source-cache-refreshed-at collector)
                          sampled-at)
                    (%collect-temperature-values
                     collector
                     (%temperature-source-cache collector)
                     sampled-at
                     on-sample
                     on-unavailable))
                  on-unavailable)
         (%collect-temperature-values
          collector
          (%temperature-source-cache collector)
          sampled-at
          on-sample
          on-unavailable))))

(in-package #:ntop.temperature.infrastructure)

 (ntop.shared.domain:define-value-type temperature-source
  (identity (error "Temperature source identity is required.") :read-only t)
  (name nil :type (or null string) :read-only t)
  (handle (error "Temperature source handle is required.") :read-only t))
 (defconstant +temperature-source-cache-ttl-seconds+ 60)
 (defclass temperature-collector ()
  ((clock :initarg :clock :reader %temperature-clock)
   (enumerate-sensors :initarg :enumerate-sensors
                      :initform nil
                      :reader %temperature-enumerate-sensors)
   (read-sensors :initarg :read-sensors
                 :initform nil
                 :reader %temperature-read-sensors)
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
    (error "The temperature reader must be a function.")))

 (defun make-temperature-collector (clock &key enumerate-sensors read-sensors)
  (make-instance 'temperature-collector
                 :clock clock
                 :enumerate-sensors enumerate-sensors
                 :read-sensors read-sensors))

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
  (ntop.shared.domain:read-monotonic-time
   (%temperature-clock collector)
   (lambda (sampled-at)
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
          on-unavailable)))
   on-unavailable))

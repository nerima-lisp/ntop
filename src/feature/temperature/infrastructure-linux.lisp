(in-package #:ntop.temperature.infrastructure)

(defun %linux-temperature-sources (descriptors)
  (mapcar
   (lambda (descriptor)
     (make-temperature-source :identity
                              (getf descriptor :identity)
                              :name
                              (getf descriptor :name)
                              :handle
                              (getf descriptor :handle)))
   descriptors))

(defun %enumerate-linux-temperature-sensors (on-sources on-unavailable)
  (ntop.platform.linux:enumerate-linux-temperature-sources
   (lambda (descriptors)
     (funcall on-sources (%linux-temperature-sources descriptors)))
   on-unavailable))

(defun %read-linux-temperature-sensors (sources on-values on-unavailable)
  (ntop.platform.linux:read-linux-temperature-values
   (mapcar
    (lambda (source)
      (list :identity
            (temperature-source-identity source)
            :handle
            (temperature-source-handle source)))
    sources)
   (lambda (millidegree-values)
     (funcall on-values
              (mapcar
               (lambda (value)
                 (cons (car value) (and (cdr value) (/ (cdr value) 1000))))
               millidegree-values)))
   on-unavailable))

(defun make-linux-temperature-collector (&key now)
  (apply #'make-temperature-collector
         (append
          (list :enumerate-sensors
                #'%enumerate-linux-temperature-sensors
                :read-sensors
                #'%read-linux-temperature-sensors)
          (when now
            (list :now now)))))

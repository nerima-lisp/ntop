(in-package #:ntop.temperature.infrastructure)

(defun %validate-darwin-iohid-api (api)
  (unless
      (and (listp api)
           (functionp (getf api :enumerate-services))
           (functionp (getf api :read-temperature)))
    (error "The Darwin IOHID API is incomplete."))
  api)

(defun %darwin-temperature-sources (services)
  (loop for (identity name handle) in services
        when (and (stringp name) (plusp (length name)))
          collect (make-temperature-source :identity identity :name name :handle handle)))

(defun %enumerate-darwin-temperature-sensors (api-loader api-cache on-sources on-unavailable)
  (let ((api
         (or (car api-cache)
             (handler-case (setf (car api-cache) (%validate-darwin-iohid-api (funcall api-loader)))
               (error ()
                 nil)))))
    (if (null api)
        (funcall on-unavailable :private-api-unavailable)
        (multiple-value-bind (sources failure)
            (handler-case (values
                           (%darwin-temperature-sources (funcall (getf api :enumerate-services)))
                           nil)
              (error ()
                (values nil t)))
          (if failure
              (funcall on-unavailable :read-failed)
              (funcall on-sources sources))))))

(defun %read-darwin-temperature-sensors (sources api-loader api-cache on-values on-unavailable)
  (let ((api
         (or (car api-cache)
             (handler-case (setf (car api-cache) (%validate-darwin-iohid-api (funcall api-loader)))
               (error ()
                 nil)))))
    (cond
      ((null api) (funcall on-unavailable :private-api-unavailable))
      ((null sources) (funcall on-unavailable :not-present))
      (t
       (funcall on-values
                (mapcar
                 (lambda (source)
                   (cons (temperature-source-identity source)
                         (handler-case (funcall (getf api :read-temperature)
                                                (temperature-source-handle source))
                           (error ()
                             nil))))
                 sources))))))

(defun make-darwin-temperature-collector (clock &key
                                                (api-loader
                                                 #'ntop.platform.darwin:load-darwin-iohid-api))
  (unless (functionp api-loader)
    (error "API-LOADER must be a function, got ~S." api-loader))
  (let ((api-cache (list nil)))
    (make-temperature-collector clock
                                :enumerate-sensors
                                (lambda (on-sources on-unavailable)
                                  (%enumerate-darwin-temperature-sensors api-loader
                                                                         api-cache
                                                                         on-sources
                                                                         on-unavailable))
                                :read-sensors
                                (lambda (sources on-values on-unavailable)
                                  (%read-darwin-temperature-sensors sources
                                                                    api-loader
                                                                    api-cache
                                                                    on-values
                                                                    on-unavailable)))))

(in-package #:ntop.cli)

(defparameter *application-runner*
  #'ntop.app.infrastructure:run-application)

#+(or darwin linux)
(defun %run-application (config)
  (let* ((clock (ntop.app.infrastructure:make-system-clock))
         (collectors
           (ntop.app.infrastructure:make-platform-collectors config clock))
         (composition
           (ntop.app.application:make-application-composition config)))
    (funcall *application-runner*
             config
             collectors
             clock
             (ntop.app.application:application-composition-render composition)
             :on-action
             (ntop.app.application:application-composition-on-action composition)
             :on-snapshot
             (ntop.app.application:application-composition-on-snapshot composition)
             :history-stores
             (ntop.app.application:application-composition-history-stores composition))
    0))

#-(or darwin linux)
(defun %run-application (config)
  (declare (ignore config))
  1)

(defun %run-cli (argv)
  (handler-case
      (let* ((app (%make-cli-app))
             (invocation (cl-cli:parse-argv app argv)))
        (case (cl-cli:invocation-action invocation)
          (:help
           (cl-cli:print-app-help app)
           0)
          (:version
           (format t "ntop 0.1.0~%")
           0)
          (otherwise
           (multiple-value-bind (config status)
               (%config-from-invocation invocation)
             (if (zerop status)
                 (%run-application config)
                 (progn
                   (format *error-output* "Error: invalid command-line input.~%")
                   status))))))
    (cl-cli:cli-usage-error (condition)
      (format *error-output* "~A~%" condition)
      2)))

(defun main (&optional argv)
  (sb-ext:exit :code (%run-cli (or argv sb-ext:*posix-argv*))))

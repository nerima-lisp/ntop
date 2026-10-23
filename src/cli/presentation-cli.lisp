(in-package #:ntop.cli)

(defparameter *application-runner*
  #'ntop.app.infrastructure:run-application)

(defun %minimal-renderer (state config snapshot columns rows reasons)
  (declare (ignore state config snapshot columns rows reasons))
  nil)

(defun %minimal-action-handler (state action)
  (declare (ignore state action))
  nil)

#+(or darwin linux)
(defun %run-application (config)
  (funcall *application-runner*
           config
           nil
           (ntop.app.infrastructure:make-system-clock)
           #'%minimal-renderer
           :on-action
           #'%minimal-action-handler)
  0)

#-(or darwin linux)
(defun %run-application (config)
  (declare (ignore config))
  1)

(defun %run-cli (argv)
  (handler-case (let* ((app (%make-cli-app))
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
                           status)))))
    (cl-cli:cli-usage-error (condition)
      (format *error-output* "~A~%" condition)
      2)))

(defun main (&optional argv)
  (sb-ext:exit :code (%run-cli (or argv sb-ext:*posix-argv*))))

(in-package #:ntop.e2e)

(define-condition measurement-error
  (error)
  ((reason :initarg :reason :reader measurement-error-reason))
  (:report
   (lambda (condition stream)
     (write-string (measurement-error-reason condition) stream))))

(define-condition measurement-unsupported
  (measurement-error)
  ())

(defparameter *measurement-rss-cadence-seconds*
  30d0)

(defparameter *measurement-frame-quiet-seconds*
  0.25d0)

(defparameter *measurement-first-frame-factor*
  3d0)

(defparameter *measurement-interaction-samples*
  20)

(defparameter *measurement-key-octets*
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element #x3f))

(defparameter *measurement-n6-query*
  (format nil "~c[?25$p" #\Escape))

(defun %measurement-fail (control &rest arguments)
  (error 'measurement-error :reason (apply #'format nil control arguments)))

(defun %measurement-unsupported (control &rest arguments)
  (error 'measurement-unsupported
         :reason
         (apply #'format nil control arguments)))

(defun %validate-positive-integer (value name)
  (unless (and (integerp value) (plusp value))
    (%measurement-fail "~a must be a positive integer." name))
  value)

(defun %validate-duration (definition duration-seconds timeout-seconds)
  (unless
      (and (realp timeout-seconds)
           (< 0 timeout-seconds)
           (< timeout-seconds most-positive-double-float))
    (%measurement-fail "TIMEOUT-SECONDS must be a finite positive duration."))
  (when (getf definition :uses-duration-p)
    (%validate-positive-integer duration-seconds "DURATION-SECONDS")
    (when (>= duration-seconds timeout-seconds)
      (%measurement-fail "TIMEOUT-SECONDS must exceed DURATION-SECONDS.")))
  timeout-seconds)

(defun %make-measurement-directory ()
  (loop for directory = (merge-pathnames
                         (format nil
                                 "ntop-measure-~d-~d/"
                                 (get-universal-time)
                                 (random 1000000000))
                         (uiop:temporary-directory))
        unless (probe-file directory)
          do (ensure-directories-exist (merge-pathnames ".claim" directory)) (let ((claim
                                                                                    (open
                                                                                     (merge-pathnames
                                                                                      ".claim"
                                                                                      directory)
                                                                                     :direction
                                                                                     :output
                                                                                     :if-exists
                                                                                     nil
                                                                                     :if-does-not-exist
                                                                                     :create)))
                                                                               (when
                                                                                   claim
                                                                                 (close
                                                                                  claim)
                                                                                 (return
                                                                                  directory)))))

(defun %write-sexp (path value)
  (with-open-file
      (stream path
              :direction
              :output
              :if-exists
              :supersede
              :if-does-not-exist
              :create
              :external-format
              :utf-8)
    (let ((*print-circle* t)
          (*print-readably* t))
      (write value :stream stream)
      (terpri stream)))
  path)

(defun %write-text (path text)
  (with-open-file
      (stream path
              :direction
              :output
              :if-exists
              :supersede
              :if-does-not-exist
              :create
              :external-format
              :utf-8)
    (write-string text stream))
  path)

(defun %write-octets (path octets)
  (with-open-file
      (stream path
              :direction
              :output
              :element-type
              '(unsigned-byte 8)
              :if-exists
              :supersede
              :if-does-not-exist
              :create)
    (write-sequence octets stream))
  path)

(defun %append-octets (destination source)
  (loop for octet across source
        do (vector-push-extend octet destination))
  destination)

(defun %octets-to-text (octets)
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun %ensure-btm (target)
  (unless (string= target "btm")
    (if (string= target "ntop")
        (%measurement-unsupported
         "ntop is not running in this WP0-6 smoke; only btm is measurable.")
        (%measurement-fail "Unknown measurement target: ~s" target)))
  "btm")

(defun %write-samples (directory name run samples)
  (%write-sexp
   (merge-pathnames (format nil "~a-run-~d.sexp" name run) directory)
   (list :measurement name :run run :samples samples)))

(defun %write-manifest (directory target measurement status reason)
  (%write-sexp (merge-pathnames "manifest.sexp" directory)
               (list :target
                     target
                     :measurement
                     measurement
                     :status
                     status
                     :reason
                     reason)))

(defmacro define-measurement-procedures (&body procedures)
  `(defparameter *measurement-procedures*
     (quote
      ,(mapcar
        (lambda (procedure)
          (list :name
                (first procedure)
                :function
                (second procedure)
                :uses-duration-p
                (third procedure)))
        procedures))))

(define-measurement-procedures ("n1" %run-n1 t)
                               ("n2" %run-n2 t)
                               ("n3" %run-n3 nil)
                               ("n4" %run-n4 nil)
                               ("n5" %run-n5 nil)
                               ("n6" %run-n6 nil))

(defun run-measurement (&key (target "btm")
                             (measurement "n1")
                             (rows 48)
                             (columns 160)
                             (rate 700)
                             (runs 1)
                             (duration-seconds 60)
                             (timeout-seconds 90d0))
  (%validate-positive-integer rows "ROWS")
  (%validate-positive-integer columns "COLUMNS")
  (%validate-positive-integer rate "RATE")
  (%validate-positive-integer runs "RUNS")
  (let* ((resolved-target (%ensure-btm target))
         (definition
          (find measurement
                *measurement-procedures*
                :key
                (lambda (entry)
                  (getf entry :name))
                :test
                #'string=)))
    (unless definition
      (%measurement-fail "Unknown measurement procedure: ~s" measurement))
    (%validate-duration definition duration-seconds timeout-seconds)
    (let* ((deadline
            (+ (%now) (%seconds-to-ticks timeout-seconds)))
           (directory nil)
           (*e2e-deadline* deadline))
      (handler-case
          (with-e2e-operation-timeout
           ((format nil "measurement ~a" measurement)
            deadline
            timeout-seconds)
           (setf directory (%make-measurement-directory))
           (%write-manifest directory resolved-target measurement :running nil)
           (funcall (symbol-function (getf definition :function))
                    directory
                    resolved-target
                    rows
                    columns
                    rate
                    runs
                    duration-seconds
                    timeout-seconds)
           (%write-manifest directory
                            resolved-target
                            measurement
                            :complete
                            nil)
           (format t
                   "~a btm measurement complete; samples: ~a~%"
                   measurement
                   (namestring directory))
           0)
        (error (condition)
          (when directory
            (with-e2e-operation-timeout
             ("failure manifest"
              (+ (%now) (%seconds-to-ticks 1d0))
              1d0)
             (%write-manifest directory
                              resolved-target
                              measurement
                              (if (typep condition 'measurement-unsupported)
                                  :unsupported
                                  :failed)
                              (princ-to-string condition))))
          (error condition))))))

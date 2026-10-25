(in-package #:ntop.platform.darwin)

(defparameter *mach-timebase-layout*
  '(:size 8 :field-size 4 :numerator-offset 0 :denominator-offset 4))
(defparameter *mach-vm-statistics64-layout*
  '(:byte-size 152 :word-count 38 :flavor 4))

(defun decode-little-endian-unsigned (octets &key
                                             (start 0)
                                             (end (length octets)))
  (check-type octets (vector (unsigned-byte 8)))
  (unless (<= 0 start end (length octets))
    (error "Invalid byte range ~D..~D for ~D octets." start end (length octets)))
  (loop with value = 0
        for offset downfrom (1- end) to start
        do (setf value (+ (ash value 8) (aref octets offset)))
        finally (return value)))

(defun decode-little-endian-signed (octets &key (start 0) (end (length octets)))
  (let* ((unsigned (decode-little-endian-unsigned octets :start start :end end))
         (bits (* 8 (- end start))))
    (if (and (plusp bits) (logbitp (1- bits) unsigned))
        (- unsigned (ash 1 bits))
        unsigned)))

(defun decode-c-string (octets &key (start 0) (end (length octets)))
  (check-type octets (vector (unsigned-byte 8)))
  (unless (<= 0 start end (length octets))
    (error "Invalid byte range ~D..~D for ~D octets." start end (length octets)))
  (sb-ext:octets-to-string octets
                           :start
                           start
                           :end
                           (or (position 0 octets :start start :end end) end)
                           :external-format
                           :utf-8))

(defun decode-mach-timebase (octets)
  (let* ((layout *mach-timebase-layout*)
         (field-size (getf layout :field-size))
         (numerator
          (decode-little-endian-unsigned octets
                                         :start
                                         (getf layout :numerator-offset)
                                         :end
                                         (+ (getf layout :numerator-offset)
                                            field-size)))
         (denominator
          (decode-little-endian-unsigned octets
                                         :start
                                         (getf layout :denominator-offset)
                                         :end
                                         (+ (getf layout :denominator-offset)
                                            field-size))))
    (unless (= (length octets) (getf layout :size))
      (error "Expected ~D timebase octets, got ~D."
             (getf layout :size)
             (length octets)))
    (when (or (zerop numerator) (zerop denominator))
      (error "Mach timebase fields must be non-zero."))
    (values numerator denominator)))

(defun mach-ticks-to-nanoseconds (ticks numerator denominator)
  (when (zerop denominator)
    (error "Mach timebase denominator must be non-zero."))
  (floor (* ticks numerator) denominator))

(defmacro define-darwin-ffi (&body declarations)
  (labels ((expand-routine (name foreign-name result-type arguments)
             (let ((optional-position (position '&optional arguments)))
               (if (null optional-position)
                   `(sb-alien:define-alien-routine (,foreign-name ,name)
                                                   ,result-type
                                                   ,@arguments)
                   (let* ((required (subseq arguments 0 optional-position))
                          (optional (subseq arguments (1+ optional-position)))
                          (required-names (mapcar #'first required))
                          (optional-names (mapcar #'first optional))
                          (supplied-p-names
                           (loop repeat (length optional)
                                 collect (gensym "OPTIONAL-SUPPLIED-P-")))
                          (routine-names
                           (loop repeat (1+ (length optional))
                                 collect (gensym
                                          (format nil
                                                  "~A-ARITY-"
                                                  (symbol-name name))))))
                     (unless (plusp (length optional))
                       (error
                        "A Darwin variadic routine needs an optional argument."))
                     (unless
                         (every
                          (lambda (argument)
                            (and (listp argument) (= (length argument) 2)))
                          optional)
                       (error
                        "Each Darwin variadic argument needs a name and type."))
                     (labels ((routine-form (arity)
                                `(sb-alien:define-alien-routine
                                  (,foreign-name ,(nth arity routine-names))
                                  ,result-type
                                  ,@required
                                  ,@(subseq optional 0 arity)))
                              (call-form (arity)
                                `(,(nth arity routine-names) ,@required-names
                                  ,@(subseq optional-names 0 arity)))
                              (dispatch-form (index)
                                (if (= index (length optional))
                                    (call-form index)
                                    `(if ,(nth index supplied-p-names)
                                         ,(dispatch-form (1+ index))
                                         ,(call-form index)))))
                       `(progn
                          ,@(loop for arity from 0 to (length optional)
                                  collect (routine-form arity))
                          (defun ,name (,@required-names &optional
                                                         ,@(loop for (argument-name
                                                                      argument-type) in optional
                                                                 for supplied-p in supplied-p-names
                                                                 collect `(,argument-name
                                                                           nil
                                                                           ,supplied-p)))
                            ,(dispatch-form 0))))))))
           (expand-declaration (declaration)
             (destructuring-bind (kind name foreign-name type &rest arguments)
                 declaration
               (ecase kind
                 (:routine (expand-routine name foreign-name type arguments))
                 (:variable
                  `(sb-alien:define-alien-variable (,foreign-name ,name) ,type))))))
    `(progn
       ,@(mapcar #'expand-declaration declarations))))

#+darwin
(define-darwin-ffi
  (:routine %sysctl-by-name "sysctlbyname" sb-alien:int
    (name sb-alien:c-string)
    (old-value sb-alien:system-area-pointer)
    (old-length (* sb-alien:unsigned-long))
    (new-value sb-alien:system-area-pointer)
    (new-length sb-alien:unsigned-long))
  (:routine %sysctl "sysctl" sb-alien:int
    (mib (* sb-alien:int))
    (mib-length sb-alien:unsigned-int)
    (old-value sb-alien:system-area-pointer)
    (old-length (* sb-alien:unsigned-long))
    (new-value sb-alien:system-area-pointer)
    (new-length sb-alien:unsigned-long))
  (:routine %mach-host-self "mach_host_self" sb-alien:unsigned-int)
  (:variable %mach-task-self "mach_task_self_" sb-alien:unsigned-int)
  (:routine %mach-port-deallocate "mach_port_deallocate" sb-alien:int
    (task sb-alien:unsigned-int)
    (name sb-alien:unsigned-int))
  (:routine %mach-timebase-info "mach_timebase_info" sb-alien:int
    (info sb-alien:system-area-pointer))
  (:routine %host-statistics64 "host_statistics64" sb-alien:int
    (host sb-alien:unsigned-int)
    (flavor sb-alien:int)
    (statistics sb-alien:system-area-pointer)
    (count (* sb-alien:unsigned-int)))
  (:routine %vm-deallocate "vm_deallocate" sb-alien:int
    (task sb-alien:unsigned-int)
    (address sb-alien:unsigned-long)
    (size sb-alien:unsigned-long))
  (:routine %getloadavg "getloadavg" sb-alien:int
    (samples sb-alien:system-area-pointer)
    (count sb-alien:int))
  (:routine %strerror "strerror" sb-alien:c-string
    (number sb-alien:int))
  (:routine %fcntl "fcntl" sb-alien:int
    (file-descriptor sb-alien:int)
    (command sb-alien:int)
    &optional
    (argument sb-alien:int)))

#+darwin
(declaim (inline %sysctl-result-octets))

#+darwin
(defun %sysctl-result-octets (buffer result-length)
  (if (= result-length (length buffer))
      buffer
      (subseq buffer 0 result-length)))

#+darwin
(defun %read-sysctl (query on-value on-error)
  (sb-alien:with-alien ((length sb-alien:unsigned-long 0))
    (let ((status (funcall query (sb-sys:int-sap 0) (sb-alien:addr length))))
      (unless (zerop status)
        (return-from %read-sysctl
          (funcall on-error
                   (list :sysctl-error status (sb-alien::get-errno)))))
      (let* ((capacity length)
             (buffer
               (make-array (max capacity 1)
                           :element-type '(unsigned-byte 8))))
        (setf length capacity)
        (sb-sys:with-pinned-objects (buffer)
          (setf status
                (funcall query (sb-sys:vector-sap buffer)
                         (sb-alien:addr length))))
        (cond
          ((not (zerop status))
           (funcall on-error
                    (list :sysctl-error status (sb-alien::get-errno))))
          ((> length capacity)
           (funcall on-error (list :sysctl-buffer-grew capacity length)))
          (t
           (funcall on-value (%sysctl-result-octets buffer length))))))))

#+darwin
(defun sysctl-read-by-name (name on-value on-error)
  (%read-sysctl
   (lambda (old-value old-length)
     (%sysctl-by-name name old-value old-length (sb-sys:int-sap 0) 0))
   on-value
   on-error))

#+darwin
(defun sysctl-read-mib (mib on-value on-error)
  (let ((code-count (length mib)))
    (when (zerop code-count)
      (error "A sysctl MIB must contain at least one integer."))
    (let ((storage (sb-alien:make-alien sb-alien:int code-count)))
      (unwind-protect
           (progn
             (loop for index below code-count
                   for code = (elt mib index)
                   do (check-type code (signed-byte 32))
                      (setf (sb-alien:deref storage index) code))
             (%read-sysctl
              (lambda (old-value old-length)
                (%sysctl storage code-count old-value old-length
                         (sb-sys:int-sap 0) 0))
              on-value
              on-error))
        (sb-alien:free-alien storage)))))

#+darwin
(defun sysctl-read-integer-by-name (name width signedp on-value on-error)
  (sysctl-read-by-name
   name
   (lambda (octets)
     (if (= (length octets) width)
         (funcall on-value
                  (if signedp
                      (decode-little-endian-signed octets)
                      (decode-little-endian-unsigned octets)))
         (funcall on-error
                  (list :sysctl-width-mismatch width (length octets)))))
   on-error))

#+darwin
(defun sysctl-read-string-by-name (name on-value on-error)
  (sysctl-read-by-name
   name
   (lambda (octets)
     (funcall on-value (decode-c-string octets)))
   on-error))

#+darwin
(defun call-with-mach-host-port (on-value on-error)
  (let ((port (%mach-host-self)))
    (if (zerop port)
        (funcall on-error :invalid-host-port)
        (unwind-protect
             (funcall on-value port)
          (let ((status (%mach-port-deallocate %mach-task-self port)))
            (unless (zerop status)
              (error "mach_port_deallocate failed with code ~D." status)))))))

#+darwin
(defun mach-read-host-vm-statistics64 (on-value on-error)
  "Read the 152-byte HOST_VM_INFO64 revision-1 prefix.
ON-VALUE receives a 152-element (vector (unsigned-byte 8)).
ON-ERROR receives :INVALID-HOST-PORT, (:MACH-ERROR status), or
(:MACH-COUNT-MISMATCH 38 actual). Port-deallocation failures signal a condition."
  (let* ((layout *mach-vm-statistics64-layout*)
         (expected-count (getf layout :word-count))
         (octets (make-array (getf layout :byte-size)
                             :element-type '(unsigned-byte 8))))
    (call-with-mach-host-port
     (lambda (host)
       (sb-alien:with-alien ((count sb-alien:unsigned-int expected-count))
         (sb-sys:with-pinned-objects (octets)
           (let ((status (%host-statistics64
                          host
                          (getf layout :flavor)
                          (sb-sys:vector-sap octets)
                          (sb-alien:addr count))))
             (if (not (zerop status))
                 (funcall on-error (list :mach-error status))
                 (let ((actual-count count))
                   (if (= actual-count expected-count)
                       (funcall on-value octets)
                       (funcall on-error
                                (list :mach-count-mismatch
                                      expected-count
                                      actual-count)))))))))
     on-error)))

#+darwin
(defun mach-timebase-info (on-value on-error)
  (let* ((layout *mach-timebase-layout*)
         (octets
           (make-array (getf layout :size)
                       :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (octets)
      (let ((status (%mach-timebase-info (sb-sys:vector-sap octets))))
        (if (zerop status)
            (multiple-value-call on-value (decode-mach-timebase octets))
            (funcall on-error (list :mach-error status)))))))

#+darwin
(defun mach-vm-deallocate (address size on-value on-error)
  (let ((status
          (%vm-deallocate %mach-task-self (sb-sys:sap-int address) size)))
    (if (zerop status)
        (funcall on-value)
        (funcall on-error (list :mach-error status)))))

#+darwin
(defun mach-read-load-average (on-value on-error)
  (sb-alien:with-alien ((samples (sb-alien:array sb-alien:double 3)))
    (let ((count
            (%getloadavg
             (sb-alien:alien-sap
              (sb-alien:cast samples (* sb-alien:double)))
             3)))
      (if (minusp count)
          (funcall on-error
                   (list :errno (sb-alien::get-errno)))
          (funcall on-value
                   (loop for index below count
                         collect (sb-alien:deref samples index)))))))

#+darwin
(defun darwin-error-string (number)
  (%strerror number))

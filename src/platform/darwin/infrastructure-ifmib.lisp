(in-package #:ntop.platform.darwin)

(defparameter *ifmibdata-layout*
  '(:size 180
    :name-offset 0
    :name-size 16
    :data-offset 52
    :counter-size 8
    :counters ((:rx-bytes . 64)
               (:tx-bytes . 72)
               (:rx-packets . 24)
               (:tx-packets . 40))))

(defun %decode-ifmibdata-counter (octets layout offset)
  (let* ((start (+ (getf layout :data-offset) offset))
         (end (+ start (getf layout :counter-size))))
    (decode-little-endian-unsigned octets :start start :end end)))

(defun %network-ifmib-errno-reason (errno)
  (if (member errno (quote (1 13)) :test (function eql))
      :permission-denied
    :read-failed))
(defun %ifmib-permission-denied-p (failure)
  (let ((reason (third failure)))
    (and (eq (first failure) :ifmib-unavailable)
         (consp reason)
         (eq (first reason) :sysctl-error)
         (member (third reason) (quote (1 13)) :test (function eql)))))
(defun %ifmib-unavailable-reason (failures)
  (if (some (function %ifmib-permission-denied-p) failures)
      :permission-denied
    :read-failed))
(defun %deliver-ifmib-results (data failures on-value on-unavailable)
  (cond
   (failures
    (funcall on-unavailable (%ifmib-unavailable-reason failures)))
   (data
    (funcall on-value (nreverse data)))
   (t
    (funcall on-unavailable :not-present))))

(defun decode-ifmibdata (octets)
  (check-type octets (vector (unsigned-byte 8)))
  (let ((layout *ifmibdata-layout*))
    (unless (= (length octets) (getf layout :size))
      (error "Expected ~D ifmibdata octets, got ~D."
             (getf layout :size)
             (length octets)))
    (append
     (list :name
           (decode-c-string octets
                            :start (getf layout :name-offset)
                            :end (+ (getf layout :name-offset)
                                    (getf layout :name-size))))
     (loop for counter in (getf layout :counters)
           append (list (car counter)
                        (%decode-ifmibdata-counter
                         octets
                         layout
                         (cdr counter)))))))

#+darwin
(define-darwin-ffi
  (:routine %if-nameindex "if_nameindex" sb-alien:system-area-pointer)
  (:routine %if-freenameindex "if_freenameindex" sb-alien:void
    (index-list sb-alien:system-area-pointer)))

#+darwin
(defparameter *if-nameindex-layout*
  '(:entry-size 16
    :index-offset 0
    :name-offset 8
    :name-size 16
    :entry-limit 4096))

#+darwin
(defun %copy-if-nameindex-name (name-pointer layout)
  (when (zerop (sb-sys:sap-int name-pointer))
    (error "if_nameindex returned a null interface name."))
  (let* ((size (getf layout :name-size))
         (octets (make-array size :element-type '(unsigned-byte 8))))
    (loop for offset below size
          for byte = (sb-sys:sap-ref-8 name-pointer offset)
          do (setf (aref octets offset) byte)
          when (zerop byte)
            do (return-from %copy-if-nameindex-name
                 (decode-c-string octets :end offset)))
    (decode-c-string octets)))

#+darwin
(defun %read-if-nameindex-entries (index-list)
  (let* ((layout *if-nameindex-layout*)
         (entries nil))
    (loop for entry-index below (getf layout :entry-limit)
          for entry = (sb-sys:sap+
                       index-list
                       (* entry-index (getf layout :entry-size)))
          for interface-index =
            (ldb (byte 32 0)
                 (sb-sys:sap-ref-32 entry (getf layout :index-offset)))
          do (cond
               ((zerop interface-index)
                (return-from %read-if-nameindex-entries
                  (values (nreverse entries) nil)))
               ((> interface-index #x7fffffff)
                (return-from %read-if-nameindex-entries
                  (values nil :interface-index-out-of-range)))
               (t
                (let ((name-pointer
                        (sb-sys:sap-ref-sap entry
                                            (getf layout :name-offset))))
                  (handler-case
                      (push (cons interface-index
                                  (%copy-if-nameindex-name name-pointer layout))
                            entries)
                    (error (condition)
                      (return-from %read-if-nameindex-entries
                        (values nil
                                (list :invalid-interface-name
                                      interface-index
                                      condition))))))))
          finally (return (values nil :if-nameindex-list-not-terminated)))))

#+darwin
(progn
  (defun %ifmib-current-errno ()
    (sb-alien::get-errno))
  (defun darwin-read-network-interface-data (on-value on-unavailable)
    (let ((index-list (%if-nameindex)))
      (if (zerop (sb-sys:sap-int index-list))
          (funcall on-unavailable
                   (%network-ifmib-errno-reason (%ifmib-current-errno)))
          (unwind-protect
               (multiple-value-bind (interfaces failure)
                   (handler-case
                       (%read-if-nameindex-entries index-list)
                     (error (condition)
                       (values nil (list :if-nameindex-error condition))))
                 (cond
                   (failure
                    (funcall on-unavailable :read-failed))
                   ((null interfaces)
                    (funcall on-unavailable :not-present))
                   (t
                    (let ((data nil)
                          (failures nil))
                      (dolist (interface interfaces)
                        (destructuring-bind (index . name) interface
                          (handler-case
                              (sysctl-read-mib
                               (list 4 18 0 2 index 1)
                               (lambda (octets)
                                 (handler-case
                                     (let ((record (decode-ifmibdata octets)))
                                       (if (string= name (getf record :name))
                                           (push record data)
                                           (push (list :interface-name-changed
                                                       index
                                                       name
                                                       (getf record :name))
                                                 failures)))
                                   (error (condition)
                                     (push (list :invalid-ifmibdata index condition)
                                           failures))))
                               (lambda (reason)
                                 (push (list :ifmib-unavailable index reason)
                                       failures)))
                            (error (condition)
                              (push (list :ifmib-read-error index condition)
                                    failures)))))
                      (%deliver-ifmib-results data
                                              failures
                                              on-value
                                              on-unavailable)))))
            (%if-freenameindex index-list))))))

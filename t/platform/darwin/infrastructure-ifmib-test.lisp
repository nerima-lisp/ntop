(in-package #:ntop.test)
#+darwin
(defun %call-with-if-nameindex-fixture (interface-index name function)
  (let ((octets (make-array 64
                            :element-type (quote (unsigned-byte 8))
                            :initial-element 0)))
    (sb-sys:with-pinned-objects (octets)
      (let ((index-list (sb-sys:vector-sap octets)))
        (setf (sb-sys:sap-ref-32 index-list 0) interface-index
              (sb-sys:sap-ref-32 index-list 16) 0
              (sb-sys:sap-ref-sap index-list 8)
              (if name
                  (sb-sys:sap+ index-list 32)
                  (sb-sys:int-sap 0)))
        (when name
          (loop for character across name
                for offset from 32
                do (setf (sb-sys:sap-ref-8 index-list offset)
                         (char-code character))))
        (funcall function index-list)))))

(cl-weave:describe "ifmib result delivery"
  (cl-weave:it "reports partial reads with one read-failed reason"
    (let ((calls 0)
          (arguments nil))
      (ntop.platform.darwin::%deliver-ifmib-results
       (list (list :name "synthetic0"
                   :rx-bytes 1
                   :tx-bytes 2
                   :rx-packets 3
                   :tx-packets 4))
       (list (list :ifmib-unavailable 2 :synthetic))
       (lambda (data)
         (declare (ignore data))
         (incf calls))
       (lambda (&rest values)
         (incf calls)
         (setf arguments values)))
      (cl-weave:expect calls :to-be 1)
      (cl-weave:expect arguments :to-equal (quote (:read-failed)))))
  (cl-weave:it "reports a sysctl permission error with one permission-denied reason"
    (let ((calls 0)
          (arguments nil))
      (ntop.platform.darwin::%deliver-ifmib-results
       nil
       (list (list :ifmib-unavailable 2 (list :sysctl-error -1 13)))
       (lambda (data)
         (declare (ignore data))
         (incf calls))
       (lambda (&rest values)
         (incf calls)
         (setf arguments values)))
      (cl-weave:expect calls :to-be 1)
      (cl-weave:expect arguments :to-equal (quote (:permission-denied)))))
  (cl-weave:it "reports an empty interface set as not-present"
    (let ((calls 0)
          (arguments nil))
      (ntop.platform.darwin::%deliver-ifmib-results
       nil nil
       (lambda (data)
         (declare (ignore data))
         (incf calls))
       (lambda (&rest values)
         (incf calls)
         (setf arguments values)))
      (cl-weave:expect calls :to-be 1)
      (cl-weave:expect arguments :to-equal (quote (:not-present)))))
  (cl-weave:it "maps Darwin permission errno values consistently"
    (cl-weave:expect
     (mapcar (function ntop.platform.darwin::%network-ifmib-errno-reason)
             (quote (1 13 22)))
     :to-equal
     (quote (:permission-denied :permission-denied :read-failed)))))
(cl-weave:describe "Darwin ifmib failure paths"
  (cl-weave:it-run-if (member :darwin *features*)
    "copies names and rejects null interface name pointers"
    (let ((name nil))
      (%call-with-if-nameindex-fixture
       1 "fixture0"
       (lambda (index-list)
         (setf name
               (ntop.platform.darwin::%copy-if-nameindex-name
                (sb-sys:sap-ref-sap index-list 8)
                ntop.platform.darwin::*if-nameindex-layout*))))
      (cl-weave:expect name :to-equal "fixture0")
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%copy-if-nameindex-name
          (sb-sys:int-sap 0)
          ntop.platform.darwin::*if-nameindex-layout*))
       :to-throw 'error)))
  (cl-weave:it-run-if (member :darwin *features*)
    "reads a sentinel-terminated entry and rejects an out-of-range index"
    (%call-with-if-nameindex-fixture
     7 "fixture0"
     (lambda (index-list)
       (multiple-value-bind (entries failure)
           (ntop.platform.darwin::%read-if-nameindex-entries index-list)
         (cl-weave:expect entries :to-equal '((7 . "fixture0")))
         (cl-weave:expect failure :to-be nil))))
    (%call-with-if-nameindex-fixture
     #x80000000 "fixture1"
     (lambda (index-list)
       (multiple-value-bind (entries failure)
           (ntop.platform.darwin::%read-if-nameindex-entries index-list)
         (cl-weave:expect entries :to-be nil)
         (cl-weave:expect failure :to-be :interface-index-out-of-range)))))
  (cl-weave:it-run-if (member :darwin *features*)
    "returns an invalid-name failure for a null entry name"
    (%call-with-if-nameindex-fixture
     9 nil
     (lambda (index-list)
       (multiple-value-bind (entries failure)
           (ntop.platform.darwin::%read-if-nameindex-entries index-list)
         (cl-weave:expect entries :to-be nil)
         (cl-weave:expect (first failure) :to-be :invalid-interface-name)
         (cl-weave:expect (second failure) :to-be 9)))))
  (cl-weave:it-run-if (member :darwin *features*)
    "reports an interface list without a sentinel as a read failure"
    (let ((ntop.platform.darwin::*if-nameindex-layout*
            '(:entry-size 16
              :index-offset 0
              :name-offset 8
              :name-size 16
              :entry-limit 1)))
      (%call-with-if-nameindex-fixture
       9 "fixture0"
       (lambda (index-list)
         (multiple-value-bind (entries failure)
             (ntop.platform.darwin::%read-if-nameindex-entries index-list)
           (cl-weave:expect entries :to-be nil)
           (cl-weave:expect failure
                            :to-be
                            :if-nameindex-list-not-terminated))))))
  (cl-weave:it-run-if (member :darwin *features*)
    "maps null if_nameindex results to a shared unavailable reason"
    (let ((errno 13))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.platform.darwin::%if-nameindex)
          (lambda () (sb-sys:int-sap 0)))
         ((symbol-function (quote ntop.platform.darwin::%ifmib-current-errno))
          (lambda () errno)))
        (flet ((read-reason ()
                 (let ((value :unset)
                       (reason :unset)
                       (calls 0))
                   (ntop.platform.darwin::darwin-read-network-interface-data
                    (lambda (data)
                      (incf calls)
                      (setf value data))
                    (lambda (unavailable)
                      (incf calls)
                      (setf reason unavailable)))
                   (cl-weave:expect calls :to-be 1)
                   (cl-weave:expect value :to-be :unset)
                   reason)))
          (cl-weave:expect (read-reason) :to-be :permission-denied)
          (setf errno 5)
          (cl-weave:expect (read-reason) :to-be :read-failed)))))
  (cl-weave:it-run-if (member :darwin *features*)
    "maps failed, raised, and empty interface enumeration outcomes"
    (let ((enumeration-mode :reported-failure)
          (free-count 0))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.platform.darwin::%if-nameindex)
          (lambda () (sb-sys:int-sap 1)))
         ((symbol-function 'ntop.platform.darwin::%if-freenameindex)
          (lambda (index-list)
            (declare (ignore index-list))
            (incf free-count)))
         ((symbol-function 'ntop.platform.darwin::%read-if-nameindex-entries)
          (lambda (index-list)
            (declare (ignore index-list))
            (case enumeration-mode
              (:reported-failure (values nil :fixture-failure))
              (:raised (error "Synthetic interface list failure."))
              (:empty (values nil nil))))))
        (flet ((read-reason ()
                 (let ((value :unset)
                       (reason :unset)
                       (calls 0))
                   (ntop.platform.darwin::darwin-read-network-interface-data
                    (lambda (data)
                      (incf calls)
                      (setf value data))
                    (lambda (unavailable)
                      (incf calls)
                      (setf reason unavailable)))
                   (cl-weave:expect calls :to-be 1)
                   (cl-weave:expect value :to-be :unset)
                   reason)))
          (cl-weave:expect (read-reason) :to-be :read-failed)
          (setf enumeration-mode :raised)
          (cl-weave:expect (read-reason) :to-be :read-failed)
          (setf enumeration-mode :empty)
          (cl-weave:expect (read-reason) :to-be :not-present)
          (cl-weave:expect free-count :to-be 3)))))
  (cl-weave:it-run-if (member :darwin *features*)
    "keeps partial sysctl reads unavailable and preserves shared reasons"
    (let ((sysctl-mode :permission)
          (interfaces '((2 . "fixture0") (3 . "fixture1")))
          (sysctl-count 0)
          (free-count 0))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.platform.darwin::%if-nameindex)
          (lambda () (sb-sys:int-sap 1)))
         ((symbol-function 'ntop.platform.darwin::%if-freenameindex)
          (lambda (index-list)
            (declare (ignore index-list))
            (incf free-count)))
         ((symbol-function 'ntop.platform.darwin::%read-if-nameindex-entries)
          (lambda (index-list)
            (declare (ignore index-list))
            (values interfaces nil)))
         ((symbol-function 'ntop.platform.darwin:sysctl-read-mib)
          (lambda (mib on-value on-error)
            (declare (ignore mib))
            (incf sysctl-count)
            (case sysctl-mode
              (:permission
               (if (= sysctl-count 1)
                   (let ((octets (%make-ifmibdata-octets)))
                     (fill octets 0 :start 0 :end 16)
                     (loop for character across "fixture0"
                           for offset from 0
                           do (setf (aref octets offset)
                                    (char-code character)))
                     (funcall on-value octets))
                   (funcall on-error '(:sysctl-error -1 13))))
              (:raised (error "Synthetic sysctl failure."))))))
        (flet ((read-outcome ()
                 (setf sysctl-count 0)
                 (let ((value :unset)
                       (reason :unset)
                       (calls 0))
                   (ntop.platform.darwin::darwin-read-network-interface-data
                    (lambda (data)
                      (incf calls)
                      (setf value data))
                    (lambda (unavailable)
                      (incf calls)
                      (setf reason unavailable)))
                   (values value reason calls))))
          (multiple-value-bind (value reason calls) (read-outcome)
            (cl-weave:expect value :to-be :unset)
            (cl-weave:expect reason :to-be :permission-denied)
            (cl-weave:expect calls :to-be 1))
          (setf sysctl-mode :raised
                interfaces '((2 . "fixture0")))
          (multiple-value-bind (value reason calls) (read-outcome)
            (cl-weave:expect value :to-be :unset)
            (cl-weave:expect reason :to-be :read-failed)
            (cl-weave:expect calls :to-be 1))
          (cl-weave:expect sysctl-count :to-be 1)
          (cl-weave:expect free-count :to-be 2)))))
  (cl-weave:it-run-if (member :darwin *features*)
    "rejects changed interface names and malformed ifmib records"
    (let ((mode :changed-name)
          (free-count 0))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.platform.darwin::%if-nameindex)
          (lambda () (sb-sys:int-sap 1)))
         ((symbol-function 'ntop.platform.darwin::%if-freenameindex)
          (lambda (index-list)
            (declare (ignore index-list))
            (incf free-count)))
         ((symbol-function 'ntop.platform.darwin::%read-if-nameindex-entries)
          (lambda (index-list)
            (declare (ignore index-list))
            (values '((2 . "fixture0")) nil)))
         ((symbol-function 'ntop.platform.darwin:sysctl-read-mib)
          (lambda (mib on-value on-error)
            (declare (ignore mib on-error))
            (case mode
              (:changed-name
               (funcall on-value (%make-ifmibdata-octets)))
              (:malformed
               (funcall on-value
                        (make-array 179
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))))))
        (flet ((read-outcome ()
                 (let ((value :unset)
                       (reason :unset)
                       (calls 0))
                   (ntop.platform.darwin::darwin-read-network-interface-data
                    (lambda (data)
                      (incf calls)
                      (setf value data))
                    (lambda (unavailable)
                      (incf calls)
                      (setf reason unavailable)))
                   (values value reason calls))))
          (multiple-value-bind (value reason calls) (read-outcome)
            (cl-weave:expect value :to-be :unset)
            (cl-weave:expect reason :to-be :read-failed)
            (cl-weave:expect calls :to-be 1))
          (setf mode :malformed)
          (multiple-value-bind (value reason calls) (read-outcome)
            (cl-weave:expect value :to-be :unset)
            (cl-weave:expect reason :to-be :read-failed)
            (cl-weave:expect calls :to-be 1))
          (cl-weave:expect free-count :to-be 2))))))

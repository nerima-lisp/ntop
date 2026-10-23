(in-package #:ntop.test)

(defparameter *network-netstat-counter-fields*
  '((:rx-bytes "Ibytes")
    (:tx-bytes "Obytes")
    (:rx-packets "Ipkts")
    (:tx-packets "Opkts")))

(defun %write-little-endian-unsigned (octets offset width value)
  (dotimes (index width)
    (setf (aref octets (+ offset index))
          (ldb (byte 8 (* index 8)) value)))
  octets)

(defun %make-ifmibdata-octets ()
  (let ((octets (make-array 180
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (loop for character across "synthetic0"
          for offset from 0
          do (setf (aref octets offset) (char-code character)))
    (%write-little-endian-unsigned octets 116 8 #x0102030405060708)
    (%write-little-endian-unsigned octets 124 8 #x1112131415161718)
    (%write-little-endian-unsigned octets 76 8 #x2122232425262728)
    (%write-little-endian-unsigned octets 92 8 #x3132333435363738)))
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


(cl-weave:describe "ifmibdata decoder"
  (cl-weave:it "decodes the fixed name and all four 64-bit counters"
    (let ((data
            (ntop.platform.darwin:decode-ifmibdata
             (%make-ifmibdata-octets))))
      (cl-weave:expect (getf data :name) :to-equal "synthetic0")
      (cl-weave:expect (getf data :rx-bytes) :to-be #x0102030405060708)
      (cl-weave:expect (getf data :tx-bytes) :to-be #x1112131415161718)
      (cl-weave:expect (getf data :rx-packets) :to-be #x2122232425262728)
      (cl-weave:expect (getf data :tx-packets) :to-be #x3132333435363738)))
  (cl-weave:it "rejects an unexpected structure size"
    (cl-weave:expect
     (lambda ()
       (ntop.platform.darwin:decode-ifmibdata
        (make-array 179 :element-type '(unsigned-byte 8)
                    :initial-element 0)))
     :to-throw
     'error)))

(defun %split-netstat-fields (line)
  (let ((fields nil)
        (start nil))
    (loop for offset from 0 to (length line)
          for separator = (or (= offset (length line))
                              (find (char line offset)
                                    '(#\Space #\Tab #\Return #\Newline)))
          do (cond
               ((and separator start)
                (push (subseq line start offset) fields)
                (setf start nil))
               ((and (not separator) (null start))
                (setf start offset))))
    (nreverse fields)))

(defun %netstat-column-positions (fields)
  (let* ((last-position (1- (length fields)))
         (name-position (position "Name" fields :test (function string-equal)))
         (counter-positions
           (loop for field in *network-netstat-counter-fields*
                 collect
                 (cons (first field)
                       (let ((position (position (second field)
                                                  fields
                                                  :test (function string-equal))))
                         (and position (- last-position position)))))))
    (when (and name-position
               (every (lambda (column) (integerp (cdr column)))
                      counter-positions))
      (cons (cons :name name-position) counter-positions))))

(defun %parse-netstat-counter (text)
  (when (and text
             (plusp (length text))
             (every #'digit-char-p text))
    (parse-integer text)))

(defun %netstat-field-at-end-offset (fields offset)
  (let ((position (- (length fields) offset 1)))
    (and (<= 0 position)
         (< position (length fields))
         (nth position fields))))
(defun %parse-netstat-output (output)
  (let ((lines (with-input-from-string (stream output)
                 (loop for line = (read-line stream nil nil)
                       while line
                       collect line)))
        (columns nil))
    (dolist (line lines)
      (unless columns
        (setf columns (%netstat-column-positions
                       (%split-netstat-fields line)))))
    (unless columns
      (error "netstat -ib returned no recognizable interface header."))
    (let* ((name-position (cdr (assoc :name columns)))
           (counter-columns (remove :name columns :key (function car)))
           (interfaces (make-hash-table :test (quote equal))))
      (dolist (line lines)
        (let* ((fields (%split-netstat-fields line))
               (name
                 (let ((token (nth name-position fields)))
                   (if (and token
                            (plusp (length token))
                            (char= (char token (1- (length token))) #\*))
                       (subseq token 0 (1- (length token)))
                       token)))
               (counters
                 (loop for column in counter-columns
                       collect
                       (cons (car column)
                             (%parse-netstat-counter
                              (%netstat-field-at-end-offset
                               fields
                               (cdr column))))))
               (counter-plist
                 (loop for counter in counters
                       append (list (car counter) (cdr counter))))
               (record (append (list :name name) counter-plist)))
          (when (and name
                     (not (string-equal name "Name"))
                     (every (lambda (counter) (integerp (cdr counter)))
                            counters)
                     (not (gethash name interfaces)))
            (setf (gethash name interfaces) record))))
      (when (zerop (hash-table-count interfaces))
        (error "netstat -ib returned no interface counters."))
      interfaces)))

(defun %network-netstat-snapshot ()
  (let* ((result (host-kit:run-program "netstat"
                                       (list "-ib")
                                       :timeout 5
                                       :max-output-characters 131072))
         (output (host-kit:process-result-stdout result)))
    (unless (and (zerop (host-kit:process-result-exit-code result))
                 (not (host-kit:process-result-stdout-truncated-p result))
                 (not (host-kit:process-result-stderr-truncated-p result)))
      (error "netstat -ib failed or exceeded its output limit."))
    (%parse-netstat-output output)))

(defun %counter-within-netstat-bracket-p (value before after)
  (<= (min before after) value (max before after)))

(cl-weave:describe "Darwin ifmib boundary"
  (cl-weave:it-run-if (member :darwin *features*)
    "keeps each interface counter within the netstat -ib bracket"
    (let ((before (%network-netstat-snapshot))
          (after nil)
          (outcome nil)
          (interfaces nil)
          (unavailable-reason nil)
          (callback-count 0))
      (unwind-protect
           (ntop.network.infrastructure:read-network-interface-data
            (make-instance (quote ntop.network.infrastructure:network-collector)
                           :clock nil)
            (lambda (value)
              (incf callback-count)
              (setf outcome :value
                    interfaces value))
            (lambda (reason)
              (incf callback-count)
              (setf outcome :unavailable
                    unavailable-reason reason)))
        (setf after (%network-netstat-snapshot)))
      (cl-weave:expect callback-count :to-be 1)
      (cl-weave:expect outcome :to-be :value)
      (cl-weave:expect (not (null interfaces)) :to-be t)
      (cl-weave:expect (> (hash-table-count before) 0) :to-be t)
      (cl-weave:expect (> (hash-table-count after) 0) :to-be t)
      (cl-weave:expect unavailable-reason :to-be nil)
      (let ((compared 0))
        (dolist (interface interfaces)
          (let* ((name (getf interface :name))
                 (before-counters (gethash name before))
                 (after-counters (gethash name after)))
            (cl-weave:expect (not (null before-counters)) :to-be t)
            (cl-weave:expect (not (null after-counters)) :to-be t)
            (incf compared)
            (dolist (field (quote (:rx-bytes :tx-bytes :rx-packets :tx-packets)))
              (cl-weave:expect
               (%counter-within-netstat-bracket-p
                (getf interface field)
                (getf before-counters field)
                (getf after-counters field))
               :to-be
               t))))
        (cl-weave:expect (> compared 0) :to-be t)))))
(cl-weave:describe "netstat interface parser"
  (cl-weave:it "handles blank address fields and netstat interface markers"
    (let* ((snapshot
             (%parse-netstat-output
              (format nil
                      "~A~%~A~%~A~%~A~%"
                      "Name Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll"
                      "test0* 16384 <Link#1> 2 0 20 3 0 30 0"
                      "test1 1500 <Link#2> addr 5 0 50 7 0 70 0"
                      "test0* 16384 <Link#1> 4 0 40 5 0 50 0")))
           (test0 (gethash "test0" snapshot))
           (test1 (gethash "test1" snapshot)))
      (cl-weave:expect (hash-table-count snapshot) :to-be 2)
      (cl-weave:expect (getf test0 :rx-bytes) :to-be 20)
      (cl-weave:expect (getf test0 :tx-bytes) :to-be 30)
      (cl-weave:expect (getf test0 :rx-packets) :to-be 2)
      (cl-weave:expect (getf test0 :tx-packets) :to-be 3)
      (cl-weave:expect (getf test1 :rx-bytes) :to-be 50)
      (cl-weave:expect (getf test1 :tx-bytes) :to-be 70)
      (cl-weave:expect (getf test1 :rx-packets) :to-be 5)
      (cl-weave:expect (getf test1 :tx-packets) :to-be 7))))

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

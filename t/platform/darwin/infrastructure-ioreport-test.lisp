(in-package #:ntop.test)

(defun %pmgr-fixture-u32 (value)
  (loop for shift from 0 below 32 by 8
        collect (ldb (byte 8 shift) value)))

(defun %pmgr-fixture-bytes (frequencies)
  (coerce
   (loop for frequency in frequencies
         append (%pmgr-fixture-u32 frequency)
         append '(17 34 51 68))
   '(vector (unsigned-byte 8))))

(cl-weave:describe "IOReport data layouts"
  (cl-weave:it-each
    ((() nil)
     ((1020000) (1020000))
     ((1260000 1512000 1800000)
      (1260000 1512000 1800000)))
    "decodes the first u32 from each eight-byte DVFS entry"
    (input expected)
    (cl-weave:expect
     (ntop.platform.darwin::decode-pmgr-dvfs-table
      (%pmgr-fixture-bytes input))
     :to-equal expected))

  (cl-weave:it "rejects an incomplete DVFS entry"
    (cl-weave:expect
     (lambda ()
       (ntop.platform.darwin::decode-pmgr-dvfs-table
        (make-array 7 :element-type '(unsigned-byte 8) :initial-element 0)))
     :to-throw 'error))

  (cl-weave:it-each
    (("ECPU000" 0 "voltage-states1-sram")
     ("ECPU030" 3 "voltage-states1-sram")
     ("PCPU000" 4 "voltage-states5-sram")
     ("PCPU050" 9 "voltage-states5-sram")
     ("PCPU100" 10 "voltage-states13-sram")
     ("PCPU150" 15 "voltage-states13-sram"))
    "maps channel ~A to its core and cluster DVFS table"
    (name expected-core expected-table)
    (multiple-value-bind (core-index table-key)
        (ntop.platform.darwin::ioreport-cpu-channel-metadata name)
      (cl-weave:expect core-index :to-be expected-core)
      (cl-weave:expect table-key :to-equal expected-table)))

  (cl-weave:it "does not map unknown or malformed CPU channels"
  (cl-weave:expect
   (every
    (lambda (name)
      (multiple-value-bind (core-index table-key)
          (ntop.platform.darwin::ioreport-cpu-channel-metadata name)
        (and (null core-index) (null table-key))))
    (list "GPU000" "PCPU160" "PCPU050x" "" nil "PCPU05a"
          "XCPU000" "ECPU040" "ECPU001" "PCPU060" "PCPU099" "PCPU151"))
   :to-be t)))
(cl-weave:describe "IOReport additional portable edges"
  (cl-weave:it "decodes empty tables and rejects malformed property layouts"
  (cl-weave:expect
   (ntop.platform.darwin::decode-pmgr-dvfs-table
    (make-array 0 :element-type '(unsigned-byte 8)))
   :to-equal nil)
  (cl-weave:expect
   (lambda ()
     (ntop.platform.darwin::decode-pmgr-dvfs-table "not octets"))
   :to-throw 'error)
  (let ((properties (make-hash-table :test #'equal)))
    (setf (gethash "voltage-states1-sram" properties)
          (%pmgr-fixture-bytes '(1000000))
          (gethash "voltage-states5-sram" properties)
          (make-array 7 :element-type '(unsigned-byte 8) :initial-element 0)
          (gethash "voltage-states13-sram" properties)
          "not octets")
    (cl-weave:expect
     (ntop.platform.darwin::%decode-pmgr-dvfs-tables properties)
     :to-equal
     '(("voltage-states1-sram" . (1000000))
       ("voltage-states5-sram" . :invalid)
       ("voltage-states13-sram" . :invalid))))
  (let ((properties (make-hash-table :test #'equal)))
    (setf (gethash "voltage-states1-sram" properties)
          (%pmgr-fixture-bytes '(0)))
    (cl-weave:expect
     (ntop.platform.darwin::%decode-pmgr-dvfs-tables properties)
     :to-equal
     '(("voltage-states1-sram" . :invalid)
       ("voltage-states5-sram" . nil)
       ("voltage-states13-sram" . nil))))
  (let ((properties (make-hash-table :test #'equal)))
    (setf (gethash "voltage-states1-sram" properties)
          (make-array 0 :element-type '(unsigned-byte 8)))
    (progn
  (cl-weave:expect
   (ntop.platform.darwin::%decode-pmgr-dvfs-tables properties)
   :to-equal
   '(("voltage-states1-sram" . :invalid)
     ("voltage-states5-sram" . nil)
     ("voltage-states13-sram" . nil)))
  (cl-weave:expect
   (ntop.platform.darwin::%decode-pmgr-dvfs-tables "not a property hash")
   :to-equal
   '(("voltage-states1-sram" . :invalid)
     ("voltage-states5-sram" . :invalid)
     ("voltage-states13-sram" . :invalid))))))

  (cl-weave:it-run-if (member :darwin *features*)
  "classifies IOReport read failures and distinguishes absent or malformed tables"
  (let ((read-mode :supported)
        (value :unset)
        (failure :unset))
    (cl-weave:with-mocked-functions
      (((symbol-function
         'ntop.platform.darwin::%read-ioreport-cpu-performance-state-data)
        (lambda ()
          (case read-mode
            (:failure (error "IOReport sampling failed."))
            (:missing '((0 (("P0" . 1)) nil)))
            (:malformed '((0 (("P0" . 1)) :invalid)))
            (t '((0 (("P0" . 1)) (1000000))))))))
      (labels ((read-data ()
                 (setf value :unset failure :unset)
                 (ntop.platform.darwin::read-darwin-cpu-performance-state-data
                  (lambda (data) (setf value data))
                  (lambda (reason) (setf failure reason)))))
        (read-data)
        (cl-weave:expect value :to-equal '((0 (("P0" . 1)) (1000000))))
        (cl-weave:expect failure :to-be :unset)
        (setf read-mode :missing)
        (read-data)
        (cl-weave:expect failure :to-be :not-present)
        (cl-weave:expect value :to-be :unset)
        (setf read-mode :malformed)
        (read-data)
        (cl-weave:expect failure :to-be :read-failed)
        (cl-weave:expect value :to-be :unset)
        (setf read-mode :failure)
        (read-data)
        (cl-weave:expect failure :to-be :private-api-unavailable)
        (cl-weave:expect value :to-be :unset)))));

  (cl-weave:it-run-if (member :darwin *features*)
  "decodes IORegistry names and reports a registry status error"
  (let ((status 0))
    (cl-weave:with-mocked-functions
      (((symbol-function 'ntop.platform.darwin::%io-registry-entry-get-name)
        (lambda (service name-buffer)
          (declare (ignore service))
          (loop for character across "pmgr"
                for index from 0
                do (setf (sb-sys:sap-ref-8 name-buffer index)
                         (char-code character)))
          (setf (sb-sys:sap-ref-8 name-buffer 4) 0)
          status)))
      (cl-weave:expect
       (ntop.platform.darwin::%ioreport-service-name 41)
       :to-equal "pmgr")
      (setf status 5)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%ioreport-service-name 41))
       :to-throw 'error))))
  (cl-weave:it-run-if (member :darwin *features*)
    "distinguishes absent pmgr data from registry failures"
    (let ((service-mode :pmgr)
          (property-mode :success)
          (properties (make-hash-table :test #'equal))
          (property-calls 0))
      (setf (gethash "voltage-states1-sram" properties)
            (%pmgr-fixture-bytes '(1000000)))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.platform.darwin::%ioreport-service-name)
          (lambda (service)
            (declare (ignore service))
            (case service-mode
              (:other "other")
              (:service-error (error "Registry name decode failed."))
              (t "pmgr"))))
         ((symbol-function 'ntop.platform.darwin:call-with-iokit-services)
          (lambda (service-class on-service on-complete on-error)
            (declare (ignore service-class))
            (case service-mode
              (:missing (funcall on-error :missing))
              (:absent (funcall on-complete))
              (:enumeration-error (error "IOKit enumeration failed."))
              (t
               (dolist (service '(41 42))
                 (funcall on-service service))
               (funcall on-complete)))))
         ((symbol-function 'ntop.platform.darwin:iokit-registry-properties)
          (lambda (service on-value on-error)
            (declare (ignore service))
            (incf property-calls)
            (case property-mode
              (:success (funcall on-value properties))
              (:unavailable (funcall on-error :unavailable))
              (:error (error "Registry properties failed."))))))
        (labels ((expect-no-tables ()
                   (cl-weave:expect
                    (ntop.platform.darwin::%read-pmgr-dvfs-tables)
                    :to-equal
                    '(("voltage-states1-sram" . nil)
                      ("voltage-states5-sram" . nil)
                      ("voltage-states13-sram" . nil))))
                 (expect-registry-error ()
                   (cl-weave:expect
                    (lambda ()
                      (ntop.platform.darwin::%read-pmgr-dvfs-tables))
                    :to-throw 'error)))
          (cl-weave:expect
           (ntop.platform.darwin::%read-pmgr-dvfs-tables)
           :to-equal
           '(("voltage-states1-sram" . (1000000))
             ("voltage-states5-sram" . nil)
             ("voltage-states13-sram" . nil)))
          (cl-weave:expect property-calls :to-be 1)
          (setf service-mode :other property-calls 0)
          (expect-no-tables)
          (cl-weave:expect property-calls :to-be 0)
          (setf service-mode :absent)
          (expect-no-tables)
          (setf service-mode :pmgr
                properties (make-hash-table :test #'equal)
                property-calls 0)
          (expect-no-tables)
          (cl-weave:expect property-calls :to-be 1)
          (setf service-mode :missing)
          (expect-registry-error)
          (setf service-mode :enumeration-error)
          (expect-registry-error)
          (setf service-mode :service-error)
          (expect-registry-error)
          (setf service-mode :pmgr
                property-mode :unavailable)
          (expect-registry-error)
          (setf property-mode :error)
          (expect-registry-error)))))
  (cl-weave:it-run-if (member :darwin *features*)
  "filters CPU channels and validates CF-backed state data"
  (let ((states '(( "DOWN" . 3) ("P0" . 7)))
        (channels
          '(("GPU" "ECPU000")
            ("CPU Core Performance States" "ECPU000")
            ("CPU Core Performance States" "GPU000")
            ("CPU Core Performance States" "PCPU000")))
        (channels-reference (sb-sys:int-sap 22))
        (key-reference (sb-sys:int-sap 21))
        (key-null nil)
        (state-count-override nil)
        (released nil))
    (cl-weave:with-mocked-functions
      (((symbol-function
         'ntop.platform.darwin::%cf-string-create-with-c-string)
        (lambda (allocator name encoding)
          (declare (ignore allocator name encoding))
          (if key-null (sb-sys:int-sap 0) key-reference)))
       ((symbol-function 'ntop.platform.darwin::%cf-dictionary-get-value)
        (lambda (dictionary key)
          (declare (ignore dictionary key))
          channels-reference))
       ((symbol-function 'ntop.platform.darwin::%cf-release)
        (lambda (reference)
          (push (sb-sys:sap-int reference) released)))
       ((symbol-function 'ntop.platform.darwin::%cf-array-get-count)
        (lambda (array)
          (declare (ignore array))
          (length channels)))
       ((symbol-function
         'ntop.platform.darwin::%cf-array-get-value-at-index)
        (lambda (array index)
          (declare (ignore array))
          index))
       ((symbol-function 'ntop.platform.darwin::%ioreport-channel-subgroup)
        (lambda (channel) (first (nth channel channels))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-channel-name)
        (lambda (channel) (second (nth channel channels))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-state-count)
        (lambda (channel)
          (declare (ignore channel))
          (or state-count-override (length states))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-state-name)
        (lambda (channel index)
          (declare (ignore channel))
          (first (nth index states))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-state-residency)
        (lambda (channel index)
          (declare (ignore channel))
          (rest (nth index states))))
       ((symbol-function 'ntop.platform.darwin:cf-value-to-lisp)
        (lambda (reference) reference)))
      (cl-weave:expect
       (ntop.platform.darwin::%ioreport-cpu-channel-data
        (sb-sys:int-sap 30)
        '(("voltage-states1-sram" . (1000000 2000000))))
       :to-equal
       '((0 (("DOWN" . 3) ("P0" . 7)) (1000000 2000000))
         (4 (("DOWN" . 3) ("P0" . 7)) nil)))
      (cl-weave:expect released :to-equal '(21))
      (setf channels-reference (sb-sys:int-sap 0)
            released nil)
      (cl-weave:expect
       (ntop.platform.darwin::%ioreport-cpu-channel-data
        (sb-sys:int-sap 30) nil)
       :to-equal nil)
      (cl-weave:expect released :to-equal '(21))
      (setf key-null t
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%ioreport-cpu-channel-data
          (sb-sys:int-sap 30) nil))
       :to-throw 'error)
      (cl-weave:expect released :to-be-null)
      (setf key-null nil
            channels-reference (sb-sys:int-sap 22)
            channels '(("CPU Core Performance States" "ECPU000"))
            states nil
            state-count-override 0
            released nil)
      (cl-weave:expect
       (ntop.platform.darwin::%ioreport-cpu-channel-data
        (sb-sys:int-sap 30) nil)
       :to-equal '((0 nil nil)))
      (setf state-count-override -1
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%ioreport-cpu-channel-data
          (sb-sys:int-sap 30) nil))
       :to-throw 'error)
      (cl-weave:expect released :to-equal '(21))
      (setf state-count-override 1
            states '((nil . 1))
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%ioreport-cpu-channel-data
          (sb-sys:int-sap 30) nil))
       :to-throw 'error)
      (cl-weave:expect released :to-equal '(21))
      (setf states '(("P0" . -1))
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%ioreport-cpu-channel-data
          (sb-sys:int-sap 30) nil))
       :to-throw 'error)
      (cl-weave:expect released :to-equal '(21)))))
  (cl-weave:it-run-if (member :darwin *features*)
  "releases IOReport references across null and successful acquisitions"
  (let ((stage nil)
        (sample-count 0)
        (released nil))
    (cl-weave:with-mocked-functions
      (((symbol-function 'ntop.platform.darwin::%read-pmgr-dvfs-tables)
        (lambda () nil))
       ((symbol-function 'ntop.platform.darwin::%cf-string-create-with-c-string)
        (lambda (allocator name encoding)
          (declare (ignore allocator encoding))
          (cond
            ((and (eq stage :group-null) (string= name "CPU Stats"))
             (sb-sys:int-sap 0))
            ((and (eq stage :subgroup-null)
                  (string= name "CPU Core Performance States"))
             (sb-sys:int-sap 0))
            ((string= name "CPU Stats") (sb-sys:int-sap 1))
            (t (sb-sys:int-sap 2)))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-copy-channels)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (if (member stage '(:group-null :subgroup-null :channels-null))
              (sb-sys:int-sap 0)
              (sb-sys:int-sap 3))))
       ((symbol-function 'ntop.platform.darwin::%cf-dictionary-create-mutable-copy)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (if (eq stage :mutable-null)
              (sb-sys:int-sap 0)
              (sb-sys:int-sap 4))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-create-subscription)
        (lambda (reporter channels subscribed identifier options)
          (declare (ignore reporter channels identifier options))
          (setf (sb-alien:deref subscribed)
                (if (eq stage :subscribed-null)
                    (sb-sys:int-sap 0)
                    (sb-sys:int-sap 9)))
          (if (eq stage :subscription-null)
              (sb-sys:int-sap 0)
              (sb-sys:int-sap 5))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-create-samples)
        (lambda (subscription channels options)
          (declare (ignore subscription channels options))
          (incf sample-count)
          (cond
            ((and (eq stage :previous-null) (= sample-count 1))
             (sb-sys:int-sap 0))
            ((and (eq stage :current-null) (= sample-count 2))
             (sb-sys:int-sap 0))
            ((= sample-count 1) (sb-sys:int-sap 6))
            (t (sb-sys:int-sap 7)))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-create-samples-delta)
        (lambda (previous current options)
          (declare (ignore previous current options))
          (if (eq stage :delta-null)
              (sb-sys:int-sap 0)
              (sb-sys:int-sap 8))))
       ((symbol-function 'ntop.platform.darwin::%ioreport-cpu-channel-data)
        (lambda (delta tables)
          (declare (ignore delta tables))
          (if (eq stage :channel-error)
              (error "Channel decode failed.")
              :sample)))
       ((symbol-function 'ntop.platform.darwin::%cf-release)
        (lambda (reference)
          (push (sb-sys:sap-int reference) released)))
       ((symbol-function 'ntop.platform.darwin::%ioreport-sleep)
        (lambda (seconds)
          (declare (ignore seconds))
          (when (eq stage :sleep-error)
            (error "Sampling delay failed."))
          nil)))
      (labels ((read-stage (new-stage)
                 (setf stage new-stage
                       sample-count 0
                       released nil)
                 (handler-case
                     (ntop.platform.darwin::%read-ioreport-cpu-performance-state-data)
                   (error () :failed))))
        (dolist (case
                 '((:group-null :failed (2))
                   (:subgroup-null :failed (1))
                   (:channels-null :failed (1 2))
                   (:mutable-null :failed (1 2 3))
                   (:subscription-null :failed (1 2 3 4 9))
                   (:previous-null :failed (1 2 3 4 5 9))
                   (:current-null :failed (1 2 3 4 5 6 9))
                   (:sleep-error :failed (1 2 3 4 5 6 9))
                   (:delta-null :failed (1 2 3 4 5 6 7 9))
                   (:channel-error :failed (1 2 3 4 5 6 7 8 9))
                   (:subscribed-null :sample (1 2 3 4 5 6 7 8))
                   (nil :sample (1 2 3 4 5 6 7 8 9))))
          (let* ((case-stage (first case))
                 (expected-result (second case))
                 (expected-releases (third case))
                 (result (read-stage case-stage)))
            (cl-weave:expect result :to-be expected-result)
            (cl-weave:expect
             (sort (copy-list released) #'<)
             :to-equal expected-releases)))))));
;
;
;
;
)


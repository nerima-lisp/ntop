(in-package #:ntop.test)

(defmacro define-linux-fixture-case-table (suite cases
                                                 format-control
                                                 bindings
                                                 &body
                                                 body)
  `(cl-weave:describe ,suite
                      (cl-weave:it-each ,cases ,format-control ,bindings ,@body)))

(defun %sysfs-network-fixture-root (fixture)
  (asdf:system-relative-pathname "ntop"
                                 (format nil
                                         "t/platform/linux/fixtures/sysfs-network/~A/"
                                         fixture)))

(defun %read-linux-network-fixture (fixture)
  (let ((result nil))
    (ntop.platform.linux::with-linux-root (%sysfs-network-fixture-root fixture)
                                          (ntop.platform.linux::read-linux-network-statistics
                                           (lambda (value)
                                             (setf result (list :value value)))
                                           (lambda (reason)
                                             (setf result (list :unavailable
                                                                reason)))))
    result))

(defun %read-linux-cpu-frequency-fixture (fixture cpu-index)
  (let ((result nil))
    (ntop.platform.linux::with-linux-root (%sysfs-network-fixture-root fixture)
                                          (ntop.platform.linux::read-linux-cpu-frequency-khz
                                           cpu-index
                                           (lambda (value)
                                             (setf result (list :value value)))
                                           (lambda (reason)
                                             (setf result (list :unavailable
                                                                reason)))))
    result))

(define-linux-fixture-case-table "Linux CPU frequency fixtures"
                                 (("complete" 0 (:value 2400000))
                                  ("complete" 1 (:value 0)))
                                 "~A cpu~D -> ~S"
                                 (fixture cpu-index expected)
                                 (cl-weave:expect
                                  (%read-linux-cpu-frequency-fixture fixture
                                                                     cpu-index)
                                  :to-equal
                                  expected))

(cl-weave:describe "Linux sysfs network fixtures"
                   (cl-weave:it
                    "reads byte and packet counters in sorted interface order"
                    (cl-weave:expect (%read-linux-network-fixture "complete")
                                     :to-equal
                                     '(:value
                                       ((:name "eth0"
                                               :rx-bytes
                                               123456789012
                                               :tx-bytes
                                               9876543210
                                               :rx-packets
                                               234567
                                               :tx-packets
                                               765432)
                                        (:name "lo"
                                               :rx-bytes
                                               0
                                               :tx-bytes
                                               0
                                               :rx-packets
                                               0
                                               :tx-packets
                                               0)))))
                   (cl-weave:it
                    "preserves zero and reports absent counters as missing"
                    (cl-weave:expect (%read-linux-network-fixture "partial")
                                     :to-equal
                                     '(:value
                                       ((:name "partial0"
                                               :rx-bytes
                                               0
                                               :tx-bytes
                                               7
                                               :rx-packets
                                               nil
                                               :tx-packets
                                               2)))))
                   (cl-weave:it
                    "reports an absent network class directory as unavailable"
                    (cl-weave:expect (%read-linux-network-fixture "absent")
                                     :to-equal
                                     '(:unavailable :unavailable)))
                   (cl-weave:it "rejects malformed network counters"
                                (cl-weave:expect
                                 (%read-linux-network-fixture "invalid-network")
                                 :to-equal
                                 '(:unavailable :invalid-data)))
                   (cl-weave:it
                    "reports an absent CPU frequency file as unavailable"
                    (cl-weave:expect
                     (%read-linux-cpu-frequency-fixture "complete" 99)
                     :to-equal
                     '(:unavailable :unavailable)))
                   (cl-weave:it "rejects malformed CPU frequency values"
                                (cl-weave:expect
                                 (%read-linux-cpu-frequency-fixture "complete"
                                                                    2)
                                 :to-equal
                                 '(:unavailable :invalid-data)))
                   (cl-weave:it "rejects a negative CPU index"
                                (cl-weave:expect
                                 (%read-linux-cpu-frequency-fixture "complete"
                                                                    -1)
                                 :to-equal
                                 '(:unavailable :invalid-cpu-index)))
                   (cl-weave:it-property
                    "round trips generated unsigned sysfs counters"
                    ((value
                      (cl-weave:gen-integer :min 0 :max 18446744073709551615)))
                    (let ((parsed nil))
                      (ntop.platform.linux::parse-linux-sysfs-counter-text
                       (format nil "~D~%" value)
                       (lambda (number)
                         (setf parsed number))
                       (lambda (reason)
                         (setf parsed reason)))
                      (cl-weave:expect parsed :to-be value)))
                   (cl-weave:it "expands fixture tables into cl-weave cases"
                                (cl-weave:expect
                                 (first
                                  (third
                                   (macroexpand-1
                                    (quote
                                     (ntop.test::define-linux-fixture-case-table
                                      "fixture"
                                      ((1))
                                      "~D"
                                      (value)
                                      (cl-weave:expect value :to-be 1))))))
                                 :to-be
                                 (quote cl-weave:it-each))))

(cl-weave:describe "Linux sysfs counter edge cases"
                   (cl-weave:it-each
                    (("" nil :invalid-data) ("1 2" nil :invalid-data)
                     ("-1" nil :invalid-data)
                     ("0" 0 nil)
                     ("+1" 1 nil))
                    "parses sysfs counter text ~S"
                    (text expected expected-reason)
                    (let ((value nil)
                          (reason nil))
                      (ntop.platform.linux::parse-linux-sysfs-counter-text text
                                                                           (lambda
                                                                               (result)
                                                                             (setf value result))
                                                                           (lambda
                                                                               (unavailable)
                                                                             (setf reason
      unavailable)))
                      (cl-weave:expect value :to-equal expected)
                      (cl-weave:expect reason :to-equal expected-reason))))

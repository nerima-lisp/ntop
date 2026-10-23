(in-package #:ntop.platform.linux)

(defparameter *linux-network-counter-files*
  '((:rx-bytes "rx_bytes") (:tx-bytes "tx_bytes")
    (:rx-packets "rx_packets")
    (:tx-packets "tx_packets")))

(defun parse-linux-sysfs-counter-text (text on-value on-unavailable)
  (let* ((fields (split-linux-fields text))
         (value
          (and (= (length fields) 1) (parse-linux-integer (first fields)))))
    (if (and value (not (minusp value)))
        (funcall on-value value)
        (funcall on-unavailable :invalid-data))))

(defun read-linux-sysfs-counter (relative-path on-value on-unavailable)
  (read-linux-file relative-path
                   (lambda (text)
                     (parse-linux-sysfs-counter-text text
                                                     on-value
                                                     on-unavailable))
                   on-unavailable))

(defun read-linux-network-counter-fields (interface fields
                                                    values
                                                    on-value
                                                    on-unavailable)
  (if (null fields)
      (funcall on-value
               (list :name
                     interface
                     :rx-bytes
                     (getf values :rx-bytes)
                     :tx-bytes
                     (getf values :tx-bytes)
                     :rx-packets
                     (getf values :rx-packets)
                     :tx-packets
                     (getf values :tx-packets)))
      (destructuring-bind (key file-name) (first fields)
        (read-linux-sysfs-counter
         (format nil "sys/class/net/~A/statistics/~A" interface file-name)
         (lambda (value)
           (read-linux-network-counter-fields interface
                                              (rest fields)
                                              (list* key value values)
                                              on-value
                                              on-unavailable))
         (lambda (reason)
           (if (eq reason :not-present)
               (read-linux-network-counter-fields interface
                                                  (rest fields)
                                                  (list* key nil values)
                                                  on-value
                                                  on-unavailable)
               (funcall on-unavailable reason)))))))

(defun read-linux-network-interfaces (interfaces values on-value on-unavailable)
  (if (null interfaces)
      (funcall on-value (nreverse values))
      (read-linux-network-counter-fields (first interfaces)
                                         *linux-network-counter-files*
                                         nil
                                         (lambda (interface-values)
                                           (read-linux-network-interfaces
                                            (rest interfaces)
                                            (cons interface-values values)
                                            on-value
                                            on-unavailable))
                                         on-unavailable)))

(defun read-linux-network-statistics (on-value on-unavailable)
  (read-linux-directory "sys/class/net"
                        (lambda (interfaces)
                          (read-linux-network-interfaces interfaces
                                                         nil
                                                         on-value
                                                         on-unavailable))
                        on-unavailable))

(defun read-linux-cpu-frequency-khz (cpu-index on-value on-unavailable)
  (if (and (integerp cpu-index) (not (minusp cpu-index)))
      (read-linux-sysfs-counter
       (format nil
               "sys/devices/system/cpu/cpu~D/cpufreq/scaling_cur_freq"
               cpu-index)
       on-value
       on-unavailable)
      (funcall on-unavailable :invalid-cpu-index)))

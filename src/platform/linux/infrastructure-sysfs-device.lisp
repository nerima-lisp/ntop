(in-package #:ntop.platform.linux)

(defun linux-sysfs-read-text (relative-path)
  (let ((text nil)
        (reason nil))
    (read-linux-file relative-path
                     (lambda (value)
                       (setf text (string-trim
                                   '(#\Space #\Tab #\Newline #\Return)
                                   value)))
                     (lambda (unavailable)
                       (setf reason unavailable)))
    (values text reason)))

(defun linux-sysfs-read-directory (relative-path)
  (let ((entries nil)
        (reason nil))
    (read-linux-directory relative-path
                          (lambda (value)
                            (setf entries value))
                          (lambda (unavailable)
                            (setf reason unavailable)))
    (values entries reason)))

(defun linux-sysfs-required-text (relative-path)
  (multiple-value-bind (text reason) (linux-sysfs-read-text relative-path)
    (cond
      (reason (values nil reason))
      ((zerop (length text)) (values nil :invalid-data))
      (t (values text nil)))))

(defun linux-sysfs-optional-text (relative-path)
  (multiple-value-bind (text reason) (linux-sysfs-read-text relative-path)
    (unless (or reason (zerop (length text)))
      text)))

(defun linux-sysfs-required-integer (relative-path)
  (multiple-value-bind (text reason) (linux-sysfs-read-text relative-path)
    (cond
      (reason (values nil reason))
      ((parse-linux-integer text))
      (t (values nil :invalid-data)))))

(defun linux-sysfs-optional-integer (relative-path)
  (multiple-value-bind (text reason) (linux-sysfs-read-text relative-path)
    (unless (or reason (null text))
      (parse-linux-integer text))))

(defun linux-sysfs-name-index (name prefix)
  (when
      (and (> (length name) (length prefix))
           (string= prefix name :end2 (length prefix)))
    (let ((index (parse-linux-integer (subseq name (length prefix)))))
      (when (and index (not (minusp index)))
        index))))

(defun linux-sysfs-indexed-names (names prefix)
  (sort
   (loop for name in names
         for index = (linux-sysfs-name-index name prefix)
         when index
           collect (cons index name))
   #'<
   :key
   #'car))

(defun linux-hwmon-input-index (name)
  (let ((length (length name)))
    (when
        (and (> length 10)
             (string= "temp" name :end2 4)
             (string= "_input" name :start2 (- length 6)))
      (let ((index (parse-linux-integer (subseq name 4 (- length 6)))))
        (when (and index (not (minusp index)))
          index)))))

(defun linux-hwmon-inputs (entries)
  (sort
   (loop for name in entries
         for index = (linux-hwmon-input-index name)
         when index
           collect (cons index name))
   #'<
   :key
   #'car))

(defun linux-hwmon-temperature-source-descriptors (directory hwmon-name inputs)
  (let ((name
         (or (linux-sysfs-optional-text (format nil "~A/name" directory))
             hwmon-name)))
    (mapcar
     (lambda (input)
       (destructuring-bind (index . filename) input
         (let* ((label-file
                 (concatenate 'string
                              (subseq filename 0 (- (length filename) 5))
                              "label"))
                (label
                 (linux-sysfs-optional-text
                  (format nil "~A/~A" directory label-file)))
                (display-label (and label (copy-seq label))))
           (when (and display-label (plusp (length display-label)))
             (setf (char display-label 0) (char-upcase (char display-label 0))))
           (list :identity
                 (format nil "hwmon/~A/~D" hwmon-name index)
                 :name
                 (if display-label
                     (format nil "~A: ~A" name display-label)
                     name)
                 :handle
                 (list :kind :hwmon :directory directory :filename filename)))))
     inputs)))

(defun linux-hwmon-device-temperature-sources (hwmon-name)
  (let ((directory (format nil "sys/class/hwmon/~A" hwmon-name)))
    (multiple-value-bind (entries reason) (linux-sysfs-read-directory directory)
      (when (and reason (not (eq reason :unavailable)))
        (return-from linux-hwmon-device-temperature-sources
          (values nil reason)))
      (let ((inputs (linux-hwmon-inputs entries)))
        (when inputs
          (return-from linux-hwmon-device-temperature-sources
            (values
             (linux-hwmon-temperature-source-descriptors directory
                                                         hwmon-name
                                                         inputs)
             nil))))
      (multiple-value-bind (device-entries device-reason)
          (linux-sysfs-read-directory (format nil "~A/device" directory))
        (when (and device-reason (not (eq device-reason :unavailable)))
          (return-from linux-hwmon-device-temperature-sources
            (values nil device-reason)))
        (let ((device-inputs (linux-hwmon-inputs device-entries)))
          (if device-inputs
              (values
               (linux-hwmon-temperature-source-descriptors
                (format nil "~A/device" directory)
                hwmon-name
                device-inputs)
               nil)
              (values nil nil)))))))

(defun linux-hwmon-temperature-sources ()
  (multiple-value-bind (directories reason)
      (linux-sysfs-read-directory "sys/class/hwmon")
    (when (and reason (not (eq reason :unavailable)))
      (return-from linux-hwmon-temperature-sources
        (values nil reason)))
    (let ((sources nil))
      (dolist
          (directory
           (mapcar #'cdr (linux-sysfs-indexed-names directories "hwmon")))
        (multiple-value-bind (device-sources device-reason)
            (linux-hwmon-device-temperature-sources directory)
          (when device-reason
            (return-from linux-hwmon-temperature-sources
              (values nil device-reason)))
          (setf sources (nconc sources device-sources))))
      (values sources nil))))

(defparameter +linux-power-supply-integer-fields+
  '(("capacity" :capacity-percent) ("cycle_count" :cycle-count)
    ("energy_now" :energy-now-microwatt-hours)
    ("energy_avg" :energy-average-microwatt-hours)
    ("energy_full" :energy-full-microwatt-hours)
    ("energy_full_design" :energy-full-design-microwatt-hours)
    ("charge_now" :charge-now-microamp-hours)
    ("charge_avg" :charge-average-microamp-hours)
    ("charge_full" :charge-full-microamp-hours)
    ("charge_full_design" :charge-full-design-microamp-hours)
    ("power_now" :power-now-microwatts)
    ("current_now" :current-now-microamps)
    ("voltage_now" :voltage-now-microvolts)
    ("voltage_avg" :voltage-average-microvolts)
    ("voltage_max_design" :voltage-max-design-microvolts)
    ("voltage_min_design" :voltage-min-design-microvolts)
    ("voltage_present" :voltage-present-microvolts)))

(defun linux-thermal-zone-temperature-source (zone index)
  (let* ((directory (format nil "sys/class/thermal/~A" zone))
         (name
          (or (linux-sysfs-optional-text (format nil "~A/type" directory)) zone)))
    (list :identity
          (format nil "thermal-zone/~A/~D" zone index)
          :name
          name
          :handle
          (list :kind :thermal-zone :directory directory :filename "temp"))))

(defun linux-thermal-zone-temperature-sources ()
  (multiple-value-bind (directories reason)
      (linux-sysfs-read-directory "sys/class/thermal")
    (when (and reason (not (eq reason :unavailable)))
      (return-from linux-thermal-zone-temperature-sources
        (values nil reason)))
    (values
     (mapcar
      (lambda (zone)
        (linux-thermal-zone-temperature-source (cdr zone) (car zone)))
      (linux-sysfs-indexed-names directories "thermal_zone"))
     nil)))

(defun enumerate-linux-temperature-sources (on-sources on-unavailable)
  (multiple-value-bind (sources reason) (linux-hwmon-temperature-sources)
    (cond
      ((and reason (not (eq reason :unavailable)))
       (funcall on-unavailable
                (cond
                  ((eq reason :root-unavailable) :unsupported-platform)
                  ((eq reason :permission-denied) :permission-denied)
                  (t :read-failed))))
      (sources (funcall on-sources sources))
      (t
       (multiple-value-bind (fallback fallback-reason)
           (linux-thermal-zone-temperature-sources)
         (if fallback-reason
             (funcall on-unavailable
                      (cond
                        ((eq fallback-reason :root-unavailable)
                         :unsupported-platform)
                        ((eq fallback-reason :permission-denied)
                         :permission-denied)
                        (t :read-failed)))
             (funcall on-sources fallback)))))))

(defun linux-temperature-source-millidegrees (source)
  (let* ((handle (getf source :handle))
         (kind (getf handle :kind))
         (directory (getf handle :directory))
         (filename (getf handle :filename)))
    (if (and (member kind '(:hwmon :thermal-zone))
             (stringp directory)
             (stringp filename))
        (linux-sysfs-required-integer (format nil "~A/~A" directory filename))
        (values nil :invalid-data))))

(defun read-linux-temperature-values (sources on-values on-unavailable)
  (if (null sources)
      (funcall on-unavailable :not-present)
      (let ((values nil))
        (dolist (source sources)
          (multiple-value-bind (millidegrees reason)
              (linux-temperature-source-millidegrees source)
            (cond
              ((null reason)
               (push (cons (getf source :identity) millidegrees) values))
              ((eq reason :root-unavailable)
               (return-from read-linux-temperature-values
                 (funcall on-unavailable :unsupported-platform)))
              ((member reason
                       '(:unavailable :invalid-data
                                      :too-large
                                      :invalid-encoding))
               (push (cons (getf source :identity) nil) values))
              ((eq reason :permission-denied)
               (return-from read-linux-temperature-values
                 (funcall on-unavailable :permission-denied)))
              (t
               (return-from read-linux-temperature-values
                 (funcall on-unavailable :read-failed))))))
        (funcall on-values (nreverse values)))))

(defun linux-power-supply-battery (name)
  (let ((directory (format nil "sys/class/power_supply/~A" name)))
    (multiple-value-bind (type reason)
        (linux-sysfs-required-text (format nil "~A/type" directory))
      (cond
        ((eq reason :unavailable) (values nil nil))
        (reason (values nil reason))
        ((not (string= type "Battery")) (values nil nil))
        (t
         (let ((present
                (linux-sysfs-optional-integer
                 (format nil "~A/present" directory))))
           (when (eql present 0)
             (return-from linux-power-supply-battery
               (values nil nil)))
           (let ((record
                  (list :name
                        (or
                         (linux-sysfs-optional-text
                          (format nil "~A/name" directory))
                         name)
                        :type
                        type)))
             (when present
               (setf record (append record (list :present present))))
             (let ((status
                    (linux-sysfs-optional-text
                     (format nil "~A/status" directory))))
               (when status
                 (setf record (append record (list :status status)))))
             (dolist (field +linux-power-supply-integer-fields+)
               (destructuring-bind (filename key) field
                 (let ((value
                        (linux-sysfs-optional-integer
                         (format nil "~A/~A" directory filename))))
                   (when value
                     (setf record (append record (list key value)))))))
             (values record nil))))))))

(defun read-linux-batteries (on-value on-unavailable)
  (multiple-value-bind (directories reason)
      (linux-sysfs-read-directory "sys/class/power_supply")
    (cond
      ((and reason (not (eq reason :unavailable)))
       (funcall on-unavailable reason))
      (t
       (let ((batteries nil))
         (dolist (directory directories)
           (multiple-value-bind (battery battery-reason)
               (linux-power-supply-battery directory)
             (when battery-reason
               (return-from read-linux-batteries
                 (funcall on-unavailable battery-reason)))
             (when battery
               (push battery batteries))))
         (funcall on-value (nreverse batteries)))))))

(defun linux-amd-gpu-card (device-directory)
  (multiple-value-bind (cards reason)
      (linux-sysfs-read-directory (format nil "~A/drm" device-directory))
    (if reason
        (values nil reason)
        (values (cdr (first (linux-sysfs-indexed-names cards "card"))) nil))))

(defun linux-amd-gpu-temperature (device-directory)
  (multiple-value-bind (hwmons reason)
      (linux-sysfs-read-directory (format nil "~A/hwmon" device-directory))
    (declare (ignore reason))
    (dolist (hwmon (mapcar #'cdr (linux-sysfs-indexed-names hwmons "hwmon")))
      (multiple-value-bind (entries entry-reason)
          (linux-sysfs-read-directory
           (format nil "~A/hwmon/~A" device-directory hwmon))
        (declare (ignore entry-reason))
        (dolist (input (linux-hwmon-inputs entries))
          (let ((temperature
                 (linux-sysfs-optional-integer
                  (format nil
                          "~A/hwmon/~A/~A"
                          device-directory
                          hwmon
                          (cdr input)))))
            (when temperature
              (return-from linux-amd-gpu-temperature
                temperature))))))))

(defun linux-amd-gpu (device)
  (let ((directory
         (format nil "sys/module/amdgpu/drivers/pci:amdgpu/~A" device)))
    (multiple-value-bind (card reason) (linux-amd-gpu-card directory)
      (cond
        ((eq reason :unavailable) (values nil nil))
        (reason (values nil reason))
        ((null card) (values nil nil))
        (t
         (let ((record
                (list :pci-address
                      device
                      :card
                      card
                      :name
                      (or
                       (linux-sysfs-optional-text
                        (format nil "~A/product_name" directory))
                       card))))
           (dolist
               (field
                '(("gpu_busy_percent" :usage-percent)
                  ("mem_info_vram_used" :vram-used-bytes)
                  ("mem_info_vram_total" :vram-total-bytes)))
             (destructuring-bind (filename key) field
               (let ((value
                      (linux-sysfs-optional-integer
                       (format nil "~A/~A" directory filename))))
                 (when value
                   (setf record (append record (list key value)))))))
           (let ((temperature (linux-amd-gpu-temperature directory)))
             (when temperature
               (setf record (append record
                                    (list :temperature-millidegrees temperature)))))
           (values record nil)))))))

(defun read-linux-amd-gpus (on-value on-unavailable)
  (multiple-value-bind (devices reason)
      (linux-sysfs-read-directory "sys/module/amdgpu/drivers/pci:amdgpu")
    (cond
      ((and reason (not (eq reason :unavailable)))
       (funcall on-unavailable reason))
      (t
       (let ((gpus nil))
         (dolist (device devices)
           (multiple-value-bind (gpu gpu-reason) (linux-amd-gpu device)
             (when gpu-reason
               (return-from read-linux-amd-gpus
                 (funcall on-unavailable gpu-reason)))
             (when gpu
               (push gpu gpus))))
         (funcall on-value (nreverse gpus)))))))

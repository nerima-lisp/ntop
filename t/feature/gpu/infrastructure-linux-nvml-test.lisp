(in-package #:ntop.test)

(cl-weave:describe "Linux GPU reader coverage"
  (cl-weave:it "reports NVML reader exceptions as read failures"
    (let ((ntop.gpu.infrastructure::*nvml-shared-library-loader*
            (lambda () nil))
          (ntop.gpu.infrastructure::*nvml-device-data-reader*
            (lambda () (error "Injected NVML read failure."))))
      (cl-weave:expect
       (multiple-value-list
        (ntop.gpu.infrastructure::%read-linux-nvidia-gpu-rows))
       :to-equal
       '(nil :read-failed))))
  (cl-weave:it-run-if
 (member :darwin *features*)
 "returns unavailable from the non-Linux NVML reader"
 (cl-weave:expect
  (multiple-value-list
   (funcall ntop.gpu.infrastructure::*nvml-device-data-reader*))
  :to-equal
  '(nil :nvml-unavailable)))

  (cl-weave:it "maps combined GPU reader failures through the shared callback"
    (let ((result nil))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.gpu.infrastructure::%linux-amd-gpu-rows)
          (lambda (on-value on-unavailable)
            (declare (ignore on-value))
            (funcall on-unavailable :read-failed)))
         ((symbol-function
           'ntop.gpu.infrastructure::%read-linux-nvidia-gpu-rows)
          (lambda () (values nil :private-api-unavailable))))
        (ntop.gpu.infrastructure::read-linux-gpu-device-data
         (lambda (rows) (setf result (list :value rows)))
         (lambda (reason) (setf result (list :unavailable reason)))))
      (cl-weave:expect result :to-equal '(:unavailable :read-failed))))

  (cl-weave:it-run-if (member :linux *features*)
    "decodes NVML fields and applies their fallback values"
    (let ((name-result "Fixture RTX")
          (name-status 0)
          (utilization-status 0)
          (memory-status 0)
          (temperature-status 0)
          (handle-status 0)
          (observations nil))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.gpu.infrastructure::%nvml-device-get-name)
          (lambda (device buffer length)
            (declare (ignore device))
            (dotimes (index length)
              (setf (sb-alien:deref buffer index) 0))
            (loop for character across name-result
                  for index from 0
                  do (setf (sb-alien:deref buffer index)
                           (char-code character)))
            name-status))
         ((symbol-function
           'ntop.gpu.infrastructure::%nvml-device-get-utilization)
          (lambda (device utilization)
            (declare (ignore device))
            (let ((fields (sb-alien:deref utilization)))
              (setf (sb-alien:slot fields 'ntop.gpu.infrastructure::gpu) 82
                    (sb-alien:slot fields 'ntop.gpu.infrastructure::memory) 17))
            utilization-status))
         ((symbol-function 'ntop.gpu.infrastructure::%nvml-device-get-memory)
          (lambda (device memory)
            (declare (ignore device))
            (let ((fields (sb-alien:deref memory)))
              (setf (sb-alien:slot fields 'ntop.gpu.infrastructure::used) 4096
                    (sb-alien:slot fields 'ntop.gpu.infrastructure::total) 16384))
            memory-status))
         ((symbol-function
           'ntop.gpu.infrastructure::%nvml-device-get-temperature)
          (lambda (device sensor-type temperature)
            (declare (ignore device sensor-type))
            (setf (sb-alien:deref temperature) 61)
            temperature-status))
         ((symbol-function
           'ntop.gpu.infrastructure::%nvml-device-get-handle-by-index)
          (lambda (index device)
            (declare (ignore index))
            (setf (sb-alien:deref device) (sb-sys:int-sap 1))
            handle-status)))
        (let ((device (sb-sys:int-sap 1)))
          (push
           (list
            (ntop.gpu.infrastructure::%nvml-device-name device 2)
            (ntop.gpu.infrastructure::%nvml-utilization-percent device)
            (multiple-value-list
             (ntop.gpu.infrastructure::%nvml-memory-values device))
            (ntop.gpu.infrastructure::%nvml-temperature-celsius device)
            (ntop.gpu.infrastructure::%nvml-device-data 2))
           observations)
          (setf name-result "")
          (push (list (ntop.gpu.infrastructure::%nvml-device-name device 2))
                observations)
          (setf name-status 1)
          (push (list (ntop.gpu.infrastructure::%nvml-device-name device 2))
                observations)
          (setf utilization-status 1)
          (push
           (list (ntop.gpu.infrastructure::%nvml-utilization-percent device))
           observations)
          (setf memory-status 1)
          (push
           (multiple-value-list
            (ntop.gpu.infrastructure::%nvml-memory-values device))
           observations)
          (setf temperature-status 1)
          (push
           (list (ntop.gpu.infrastructure::%nvml-temperature-celsius device))
           observations)
          (setf handle-status 1)
          (push (list (ntop.gpu.infrastructure::%nvml-device-data 2))
                observations)))
      (cl-weave:expect
       (nreverse observations)
       :to-equal
       '(("Fixture RTX" 82 (4096 16384) 61
          (:name "Fixture RTX"
           :usage-percent 82
           :memory-used-bytes 4096
           :memory-total-bytes 16384
           :temperature-celsius 61))
         ("NVIDIA GPU 2")
         ("NVIDIA GPU 2")
         (nil)
         (nil)
         (nil)
         (nil)))))

  (cl-weave:it-run-if (member :linux *features*)
    "shuts down NVML after initialized queries"
    (let ((initialization-status 1)
          (count-status 0)
          (device-count 2)
          (shutdown-count 0)
          (observations nil))
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.gpu.infrastructure::%nvml-init)
          (lambda () initialization-status))
         ((symbol-function 'ntop.gpu.infrastructure::%nvml-shutdown)
          (lambda ()
            (incf shutdown-count)
            0))
         ((symbol-function
           'ntop.gpu.infrastructure::%nvml-device-get-count)
          (lambda (count)
            (setf (sb-alien:deref count) device-count)
            count-status))
         ((symbol-function 'ntop.gpu.infrastructure::%nvml-device-data)
          (lambda (index)
            (when (= index 1)
              (list :name "NVIDIA Fixture")))))
        (push
         (list
          (multiple-value-list
           (ntop.gpu.infrastructure::%query-linux-nvml-device-data))
          shutdown-count)
         observations)
        (setf initialization-status 0
              count-status 1)
        (push
         (list
          (multiple-value-list
           (ntop.gpu.infrastructure::%query-linux-nvml-device-data))
          shutdown-count)
         observations)
        (setf count-status 0)
        (push
         (list
          (multiple-value-list
           (ntop.gpu.infrastructure::%query-linux-nvml-device-data))
          shutdown-count)
         observations))
      (cl-weave:expect
       (nreverse observations)
       :to-equal
       '(((nil :nvml-unavailable) 0)
         ((nil :nvml-unavailable) 1)
         ((((:name "NVIDIA Fixture")) nil) 2)))))

  (cl-weave:it-run-if (member :linux *features*)
    "uses AMD fixtures and propagates the AMD failure callback"
    (let* ((collector
             (make-instance 'ntop.gpu.infrastructure::gpu-collector
                            :clock (make-instance 'gpu-fixture-clock)
                            :config
                            (ntop.shared.application:make-application-config)))
           (ntop.gpu.infrastructure::*nvml-shared-library-loader*
             (lambda () (error "Injected missing NVML library.")))
           (ntop.gpu.infrastructure::*nvml-device-data-reader*
             (lambda () (values nil nil)))
           (result nil))
      (ntop.platform.linux:with-linux-root
       (%gpu-linux-fixture-root "complete")
       (ntop.gpu.infrastructure::read-gpu-device-data
        collector
        (lambda (rows)
          (setf result (list :value rows)))
        (lambda (reason)
          (setf result (list :unavailable reason)))))
      (cl-weave:expect (first result) :to-be :value)
      (cl-weave:expect (length (second result)) :to-be 2)
      (setf result nil)
      (cl-weave:with-mocked-functions
        (((symbol-function 'ntop.platform.linux:read-linux-amd-gpus)
          (lambda (on-value on-unavailable)
            (declare (ignore on-value))
            (funcall on-unavailable :permission-denied))))
        (ntop.gpu.infrastructure::read-gpu-device-data
         collector
         (lambda (rows)
           (setf result (list :value rows)))
         (lambda (reason)
           (setf result (list :unavailable reason)))))
      (cl-weave:expect result :to-equal '(:unavailable :permission-denied)))))

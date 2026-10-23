(in-package #:ntop.gpu.infrastructure)

(defparameter +linux-nvml-library+ "libnvidia-ml.so.1")

(defconstant +nvml-device-name-buffer-size+
  128)

(defparameter *nvml-shared-library-loader*
  (lambda ()
    (sb-alien:load-shared-object +linux-nvml-library+ :dont-save t)))

#+linux
(progn
(sb-alien:define-alien-type nil
                            (sb-alien:struct nvml-utilization
                                             (gpu sb-alien:unsigned-int)
                                             (memory sb-alien:unsigned-int)))

(sb-alien:define-alien-type nil
                            (sb-alien:struct nvml-memory
                                             (total sb-alien:unsigned-long-long)
                                             (free sb-alien:unsigned-long-long)
                                             (used sb-alien:unsigned-long-long)))

(sb-alien:define-alien-routine ("nvmlInit_v2" %nvml-init) sb-alien:int)

(sb-alien:define-alien-routine ("nvmlShutdown" %nvml-shutdown) sb-alien:int)

(sb-alien:define-alien-routine ("nvmlDeviceGetCount_v2" %nvml-device-get-count)
                               sb-alien:int
                               (count (* sb-alien:unsigned-int)))

(sb-alien:define-alien-routine
 ("nvmlDeviceGetHandleByIndex_v2" %nvml-device-get-handle-by-index)
 sb-alien:int
 (index sb-alien:unsigned-int)
 (device (* sb-alien:system-area-pointer)))

(sb-alien:define-alien-routine ("nvmlDeviceGetName" %nvml-device-get-name)
                               sb-alien:int
                               (device sb-alien:system-area-pointer)
                               (name (* sb-alien:char))
                               (length sb-alien:unsigned-int))

(sb-alien:define-alien-routine
 ("nvmlDeviceGetUtilizationRates" %nvml-device-get-utilization)
 sb-alien:int
 (device sb-alien:system-area-pointer)
 (utilization (* (sb-alien:struct nvml-utilization))))

(sb-alien:define-alien-routine
 ("nvmlDeviceGetMemoryInfo" %nvml-device-get-memory)
 sb-alien:int
 (device sb-alien:system-area-pointer)
 (memory (* (sb-alien:struct nvml-memory))))

(sb-alien:define-alien-routine
 ("nvmlDeviceGetTemperature" %nvml-device-get-temperature)
 sb-alien:int
 (device sb-alien:system-area-pointer)
 (sensor-type sb-alien:unsigned-int)
 (temperature (* sb-alien:unsigned-int)))

(defun %nvml-device-name (device index)
  (sb-alien:with-alien
   ((buffer (sb-alien:array sb-alien:char +nvml-device-name-buffer-size+)))
   (if (zerop
        (%nvml-device-get-name device
                               (sb-alien:addr (sb-alien:deref buffer 0))
                               +nvml-device-name-buffer-size+))
       (let ((name
              (coerce
               (loop for position below +nvml-device-name-buffer-size+
                     for character = (sb-alien:deref buffer position)
                     until (char= character #\Null)
                     collect character)
               'string)))
         (if (plusp (length name))
             name
             (format nil "NVIDIA GPU ~D" index)))
       (format nil "NVIDIA GPU ~D" index))))

(defun %nvml-utilization-percent (device)
  (sb-alien:with-alien ((utilization (sb-alien:struct nvml-utilization)))
                       (when
                           (zerop
                            (%nvml-device-get-utilization device
                                                          (sb-alien:addr
                                                           utilization)))
                         (sb-alien:slot utilization 'gpu))))

(defun %nvml-memory-values (device)
  (sb-alien:with-alien ((memory (sb-alien:struct nvml-memory)))
                       (when
                           (zerop
                            (%nvml-device-get-memory device
                                                     (sb-alien:addr memory)))
                         (values (sb-alien:slot memory 'used)
                                 (sb-alien:slot memory 'total)))))

(defun %nvml-temperature-celsius (device)
  (sb-alien:with-alien ((temperature sb-alien:unsigned-int 0))
                       (when
                           (zerop
                            (%nvml-device-get-temperature device
                                                          0
                                                          (sb-alien:addr
                                                           temperature)))
                         temperature)))

(defun %nvml-device-data (index)
  (sb-alien:with-alien ((device sb-alien:system-area-pointer))
                       (unless
                           (zerop
                            (%nvml-device-get-handle-by-index index
                                                              (sb-alien:addr
                                                               device)))
                         (return-from %nvml-device-data
                           nil))
                       (multiple-value-bind (memory-used memory-total)
                           (%nvml-memory-values device)
                         (list :name
                               (%nvml-device-name device index)
                               :usage-percent
                               (%nvml-utilization-percent device)
                               :memory-used-bytes
                               memory-used
                               :memory-total-bytes
                               memory-total
                               :temperature-celsius
                               (%nvml-temperature-celsius device)))))

(defun %query-linux-nvml-device-data ()
  (let ((initialization-status (%nvml-init)))
    (unless (zerop initialization-status)
      (return-from %query-linux-nvml-device-data
        (values nil :nvml-unavailable)))
    (unwind-protect
        (sb-alien:with-alien ((count sb-alien:unsigned-int 0))
                             (if (zerop
                                  (%nvml-device-get-count (sb-alien:addr count)))
                                 (values
                                  (loop for index below
                                        count
                                        for device = (%nvml-device-data index)
                                        when device
                                          collect device)
                                  nil)
                                 (values nil :nvml-unavailable)))
      (%nvml-shutdown))))

(defparameter *nvml-device-data-reader*
  #'%query-linux-nvml-device-data))

#-linux
(defparameter *nvml-device-data-reader*
  (lambda ()
    (values nil :nvml-unavailable)))

(defun %read-linux-nvidia-gpu-rows ()
  (handler-case (funcall *nvml-shared-library-loader*)
    (error ()
      (return-from %read-linux-nvidia-gpu-rows
        (values nil :private-api-unavailable))))
  (handler-case (funcall *nvml-device-data-reader*)
    (error ()
      (values nil :read-failed))))

(defun %linux-amd-gpu-row (gpu)
  (list :name
        (getf gpu :name)
        :usage-percent
        (getf gpu :usage-percent)
        :memory-used-bytes
        (getf gpu :vram-used-bytes)
        :memory-total-bytes
        (getf gpu :vram-total-bytes)
        :temperature-celsius
        (let ((millidegrees (getf gpu :temperature-millidegrees)))
          (when millidegrees
            (/ millidegrees 1000.0)))))

(defun %linux-amd-gpu-rows (on-value on-unavailable)
  (ntop.platform.linux:read-linux-amd-gpus
   (lambda (gpus)
     (funcall on-value (mapcar #'%linux-amd-gpu-row gpus)))
   on-unavailable))

(defun %linux-gpu-unavailable-reason (amd-reason nvml-reason)
  (cond
    ((or (eq amd-reason :permission-denied) (eq nvml-reason :permission-denied))
     :permission-denied)
    ((or (eq amd-reason :root-unavailable)
         (eq amd-reason :unsupported-platform)
         (eq nvml-reason :unsupported-platform)) :unsupported-platform)
    ((and amd-reason (not (eq amd-reason :unavailable))) :read-failed)
    ((eq nvml-reason :private-api-unavailable) :private-api-unavailable)
    ((eq nvml-reason :not-present) :not-present)
    (nvml-reason :read-failed)
    (t :not-present)))

(defun read-linux-gpu-device-data (on-value on-unavailable)
  (let ((amd-gpus nil)
        (amd-reason nil)
        (nvidia-gpus nil)
        (nvml-reason nil))
    (%linux-amd-gpu-rows
     (lambda (gpus)
       (setf amd-gpus gpus))
     (lambda (reason)
       (setf amd-reason reason)))
    (multiple-value-setq (nvidia-gpus nvml-reason) (%read-linux-nvidia-gpu-rows))
    (let ((gpus (append amd-gpus nvidia-gpus)))
      (if gpus
          (funcall on-value gpus)
          (funcall on-unavailable
                   (%linux-gpu-unavailable-reason amd-reason nvml-reason))))))

#+linux
(defmethod read-gpu-device-data
    ((collector gpu-collector) on-value on-unavailable)
  (declare (ignore collector))
  (read-linux-gpu-device-data on-value on-unavailable))

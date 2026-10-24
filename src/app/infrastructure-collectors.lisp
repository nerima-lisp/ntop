(in-package #:ntop.app.infrastructure)

(defun make-platform-collectors (config clock)
  (check-type config ntop.shared.application:application-config)
  (let ((enable-cache-memory-p
         (ntop.shared.application:application-config-enable-cache-memory-p
          config)))
    (append
     (list :cpu
           (ntop.cpu.infrastructure:make-cpu-collector :clock clock)
           :memory
           (ntop.memory.infrastructure:make-memory-collector :clock
                                                             clock
                                                             :enable-cache-memory-p
                                                             enable-cache-memory-p))
     (unless (ntop.shared.application:application-config-disable-gpu-p config)
       (list :gpu (ntop.gpu.infrastructure:make-gpu-collector clock config)))
     (list :network
           (ntop.network.infrastructure:make-network-collector clock)
           :disk
           (ntop.disk.infrastructure:make-disk-collector clock))
     #+darwin
     (list
      :temperature
      (ntop.temperature.infrastructure:make-darwin-temperature-collector clock))
     #+linux
     (list
      :temperature
      (ntop.temperature.infrastructure:make-linux-temperature-collector clock))
     (when (ntop.shared.application:application-config-battery-p config)
       (list :battery
             (ntop.battery.infrastructure:make-battery-collector clock)))
     (list :process
           (ntop.process.infrastructure:make-process-collector clock config)))))

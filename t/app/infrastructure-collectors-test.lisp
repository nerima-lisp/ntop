(in-package #:ntop.test)

(defun %mock-platform-collector-assembly (config clock)
  (let ((calls nil)
        (collectors nil))
    (labels ((record-collector (key received-clock &optional settings)
               (push (list key received-clock settings) calls)
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 nil)))
      (cl-weave:with-mocked-functions
       (((symbol-function 'ntop.cpu.infrastructure:make-cpu-collector)
         (lambda (&key clock)
           (record-collector :cpu clock)))
        ((symbol-function 'ntop.memory.infrastructure:make-memory-collector)
         (lambda (&key clock enable-cache-memory-p)
           (record-collector :memory clock enable-cache-memory-p)))
        ((symbol-function 'ntop.gpu.infrastructure:make-gpu-collector)
         (lambda (clock config)
           (record-collector :gpu clock config)))
        ((symbol-function 'ntop.network.infrastructure:make-network-collector)
         (lambda (clock)
           (record-collector :network clock)))
        ((symbol-function 'ntop.disk.infrastructure:make-disk-collector)
         (lambda (clock)
           (record-collector :disk clock)))
        #+darwin
        ((symbol-function
          'ntop.temperature.infrastructure:make-darwin-temperature-collector)
         (lambda (clock &key api-loader)
           (declare (ignore api-loader))
           (record-collector :darwin-temperature clock)))
        #+linux
        ((symbol-function
          'ntop.temperature.infrastructure:make-linux-temperature-collector)
         (lambda (clock)
           (record-collector :linux-temperature clock)))
        ((symbol-function 'ntop.battery.infrastructure:make-battery-collector)
         (lambda (clock)
           (record-collector :battery clock)))
        ((symbol-function 'ntop.process.infrastructure:make-process-collector)
         (lambda (clock config)
           (record-collector :process clock config))))
       (setf collectors (ntop.app.infrastructure:make-platform-collectors config
                                                                          clock)))
      (values collectors (nreverse calls)))))

(cl-weave:describe "platform collector assembly"
                   (cl-weave:it
                    "shares one clock across enabled platform collectors"
                    (let* ((clock (list :clock))
                           (config
                            (ntop.shared.application:make-application-config
                             :enable-cache-memory-p
                             t
                             :battery-p
                             t))
                           (collectors nil)
                           (calls nil))
                      (multiple-value-setq (collectors calls) (%mock-platform-collector-assembly
                                                               config
                                                               clock))
                      (cl-weave:expect
                       (every
                        (lambda (call)
                          (eq (second call) clock))
                        calls)
                       :to-be
                       t)
                      (cl-weave:expect
                       (loop for (key value) on collectors by #'cddr
                             when value
                               collect key)
                       :to-equal
                       '(:cpu :memory
                              :gpu
                              :network
                              :disk
                              :temperature
                              :battery
                              :process))
                      (cl-weave:expect (third (find :memory calls :key #'first))
                                       :to-equal
                                       t)
                      (cl-weave:expect (third (find :gpu calls :key #'first))
                                       :to-be
                                       config)
                      (cl-weave:expect
                       (third (find :process calls :key #'first))
                       :to-be
                       config)
                      #+darwin
      (cl-weave:expect
       (not (null (assoc :darwin-temperature calls)))
       :to-be
       t)
                      #+linux
      (cl-weave:expect
       (not (null (assoc :linux-temperature calls)))
       :to-be
       t)))
                   (cl-weave:it
                    "does not create disabled GPU or battery collectors"
                    (let* ((clock (list :clock))
                           (config
                            (ntop.shared.application:make-application-config
                             :disable-gpu-p
                             t))
                           (collectors nil)
                           (calls nil))
                      (multiple-value-setq (collectors calls) (%mock-platform-collector-assembly
                                                               config
                                                               clock))
                      (cl-weave:expect (getf collectors :gpu) :to-equal nil)
                      (cl-weave:expect (getf collectors :battery) :to-equal nil)
                      (cl-weave:expect (member :gpu (mapcar #'first calls))
                                       :to-equal
                                       nil)
                      (cl-weave:expect (member :battery (mapcar #'first calls))
                                       :to-equal
                                       nil))))

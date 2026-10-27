(in-package #:ntop.test)

#+darwin
(cl-weave:describe
 "Darwin IOAccelerator reader"
 (cl-weave:it-run-if (member :darwin *features*)
   "dispatches the GPU collector to Darwin with one contract result"
   (let ((continuation-count 0))
     (ntop.gpu.infrastructure:read-gpu-device-data
      (ntop.gpu.infrastructure:make-gpu-collector nil nil)
      (lambda (rows)
        (incf continuation-count)
        (cl-weave:expect
         (every (lambda (row)
                  (equal (loop for (key value) on row by #'cddr collect key)
                         '(:name :usage-percent :memory-used-bytes
                           :memory-total-bytes :temperature-celsius)))
                rows)
         :to-be
         t))
      (lambda (reason)
        (cl-weave:expect
         (and (keywordp reason)
              (member
               reason
               '(:not-present :disabled :permission-denied
                 :unsupported-platform :private-api-unavailable :read-failed)))
         :to-be
         t)
        (incf continuation-count)))
     (cl-weave:expect continuation-count :to-be 1)))
 (cl-weave:it
   "forwards Darwin GPU rows and unavailable reasons"
   (let ((collector (ntop.gpu.infrastructure:make-gpu-collector nil nil))
         (result :unset)
         (callback-count 0))
     (cl-weave:with-mocked-functions
       (((symbol-function 'ntop.platform.darwin:darwin-read-gpu-device-data)
         (lambda (on-value on-unavailable)
           (declare (ignore on-unavailable))
           (funcall
            on-value
            '((:name "GPU 0" :usage-percent 20.0
               :memory-used-bytes 30 :memory-total-bytes 40
               :temperature-celsius 50.0))))))
       (ntop.gpu.infrastructure:read-gpu-device-data
        collector
        (lambda (rows)
          (incf callback-count)
          (setf result (list :value rows)))
        (lambda (reason)
          (incf callback-count)
          (setf result (list :unavailable reason)))))
     (cl-weave:expect
      result
      :to-equal
      '(:value ((:name "GPU 0" :usage-percent 20.0
                 :memory-used-bytes 30 :memory-total-bytes 40
                 :temperature-celsius 50.0))))
     (cl-weave:expect callback-count :to-be 1)
     (cl-weave:with-mocked-functions
       (((symbol-function 'ntop.platform.darwin:darwin-read-gpu-device-data)
         (lambda (on-value on-unavailable)
           (declare (ignore on-value))
           (funcall on-unavailable :permission-denied))))
       (setf result :unset
             callback-count 0)
       (ntop.gpu.infrastructure:read-gpu-device-data
        collector
        (lambda (rows)
          (incf callback-count)
          (setf result (list :value rows)))
        (lambda (reason)
          (incf callback-count)
          (setf result (list :unavailable reason)))))
     (cl-weave:expect result :to-equal '(:unavailable :permission-denied))
     (cl-weave:expect callback-count :to-be 1))))

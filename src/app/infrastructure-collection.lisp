(in-package #:ntop.app.infrastructure)

(defstruct collection-worker channel thread stop-flag wakeup)

(defun %read-monotonic-time (clock)
  (let ((completion (cl-concurrent-kit:make-semaphore))
        (value nil)
        (unavailable-p nil)
        (reason nil))
    (ntop.shared.domain:read-monotonic-time
     clock
     (lambda (time)
       (setf value time)
       (cl-concurrent-kit:signal-semaphore completion))
     (lambda (unavailable-reason)
       (setf unavailable-p t
             reason unavailable-reason)
       (cl-concurrent-kit:signal-semaphore completion)))
    (cl-concurrent-kit:wait-on-semaphore completion)
    (when unavailable-p
      (error "Monotonic time is unavailable: ~S." reason))
    value))

(defun %collect-snapshot (collectors clock)
  (let ((completion (cl-concurrent-kit:make-semaphore))
        (result nil)
        (unavailable-p nil)
        (reason nil))
    (ntop.app.application:collect-application-result
     collectors clock
     (lambda (value)
       (setf result value)
       (cl-concurrent-kit:signal-semaphore completion))
     (lambda (unavailable-reason)
       (setf unavailable-p t
             reason unavailable-reason)
       (cl-concurrent-kit:signal-semaphore completion)))
    (cl-concurrent-kit:wait-on-semaphore completion)
    (when unavailable-p
      (error "Collection snapshot is unavailable: ~S." reason))
    result))

(defun %publish-latest (channel value)
  (handler-case
      (unless (cl-concurrent-kit:try-send channel value)
        (cl-concurrent-kit:try-recv channel)
        (cl-concurrent-kit:try-send channel value))
    (cl-concurrent-kit:channel-closed () nil)))

(defun start-collection-worker (collectors clock rate-ms)
  (let* ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
         (stop-flag (cl-concurrent-kit:make-atomic-counter))
         (wakeup (cl-concurrent-kit:make-semaphore))
         (thread
           (cl-concurrent-kit:make-thread
            (lambda ()
              (unwind-protect
                   (handler-case
                       (ntop.app.application:run-fixed-rate-sampler
                        rate-ms
                        (lambda () (%read-monotonic-time clock))
                        (lambda (seconds)
                          (cl-concurrent-kit:wait-on-semaphore
                           wakeup :timeout seconds))
                        (lambda () (%collect-snapshot collectors clock))
                        (lambda (snapshot) (%publish-latest channel snapshot))
                        (lambda ()
                          (plusp
                           (cl-concurrent-kit:atomic-counter-value stop-flag))))
                     (error (condition)
                       (%publish-latest channel (cons :failure condition))))
                (cl-concurrent-kit:close-channel channel)))
            :name "ntop collection")))
    (make-collection-worker :channel channel
                            :thread thread
                            :stop-flag stop-flag
                            :wakeup wakeup)))

(defun stop-collection-worker (worker)
  (cl-concurrent-kit:atomic-counter-incf
   (collection-worker-stop-flag worker))
  (cl-concurrent-kit:signal-semaphore
   (collection-worker-wakeup worker))
  (cl-concurrent-kit:join-thread
   (collection-worker-thread worker)))

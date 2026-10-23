(in-package #:ntop.test)

(defun %chart-test-signals-error-p (thunk)
  (handler-case (progn
                  (funcall thunk)
                  nil)
    (error ()
      t)))

(defun %chart-test-samples (entries)
  (let ((store
         (ntop.shared.domain:make-history-store 1
                                                :retention-seconds
                                                100d0
                                                :sample-interval-seconds
                                                1d0
                                                :initial-window-seconds
                                                10d0
                                                :minimum-window-seconds
                                                1d0
                                                :zoom-step-seconds
                                                1d0)))
    (dolist (entry entries)
      (ntop.shared.domain:history-append store
                                         0
                                         (ntop.shared.domain:make-sample-record
                                          :sampled-at
                                          (first entry))
                                         (second entry)))
    (if entries
        (ntop.shared.domain:history-slice store
                                          0
                                          :start
                                          (first (first entries))
                                          :end
                                          (first (car (last entries))))
        #())))

(defun %chart-test-screen-text (screen height)
  (with-output-to-string (output)
    (loop for row below height
          do (write-string (cl-tty-kit:screen-row-string screen row) output) (terpri
                                                                              output))))

(defun %chart-test-rectangle (x y width height)
  (ntop.ui:make-rectangle :x x :y y :width width :height height))


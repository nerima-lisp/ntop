(in-package #:ntop.test)

(defun %f3-gpu-presentation-series (name value)
  "Builds a GPU chart series with one deterministic sample."
  (let ((history
          (ntop.shared.domain:make-history-store
           1
           :retention-seconds 4d0
           :sample-interval-seconds 1d0
           :initial-window-seconds 4d0
           :minimum-window-seconds 1d0)))
    (ntop.shared.domain:history-append
     history
     0
     (ntop.shared.domain:make-sample-record :sampled-at 1d0)
     value)
    (ntop.ui:make-chart-series
     :name name
     :samples (ntop.shared.domain:history-slice history 0))))

(defun %f3-gpu-screen-rows (screen)
  "Returns rendered screen rows without trailing padding."
  (loop for row below (cl-tty-kit:screen-height screen)
        collect (string-right-trim
                 '(#\Space)
                 (cl-tty-kit:screen-row-string screen row))))

(defun %f3-gpu-screen-golden-lines ()
  "Reads the expected GPU presentation rows from the golden file."
  (with-open-file (stream
                   (asdf:system-relative-pathname
                    "ntop/test"
                    "t/feature/gpu/goldens/presentation-40x16.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

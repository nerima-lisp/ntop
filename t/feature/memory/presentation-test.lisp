(in-package #:ntop.test)

(defun %f2-memory-history (values)
  (let ((history
          (ntop.shared.domain:make-history-store
           1
           :retention-seconds 4d0
           :sample-interval-seconds 1d0
           :initial-window-seconds 4d0
           :minimum-window-seconds 1d0)))
    (loop for value in values
          for index from 0
          do (ntop.shared.domain:history-append
              history
              0
              (ntop.shared.domain:make-sample-record
               :sampled-at (float (1+ index) 1d0))
              value))
    (ntop.shared.domain:history-slice history 0)))

(defun %f2-memory-fixture-series ()
  (let ((gib (* 1024 1024 1024)))
    (list :ram-samples (%f2-memory-history (list (* 4 gib) (* 5 gib) (* 6 gib)))
          :ram-used-bytes (* 6 gib)
          :ram-total-bytes (* 8 gib)
          :swap-samples (%f2-memory-history (list (* 1/2 gib) (* 3/4 gib) gib))
          :swap-used-bytes gib
          :swap-total-bytes (* 2 gib)
          :cache-samples (%f2-memory-history (list gib (* 3/2 gib) (* 2 gib)))
          :cache-used-bytes (* 2 gib)
          :cache-total-bytes (* 8 gib))))

(defun %f2-memory-screen-lines (screen)
  (loop for row below (cl-tty-kit:screen-height screen)
        collect (string-right-trim
                 '(#\Space)
                 (cl-tty-kit:screen-row-string screen row))))

(defun %f2-memory-screen-golden-lines ()
  (with-open-file (stream
                   (asdf:system-relative-pathname
                    "ntop/test"
                    "t/feature/memory/goldens/presentation-40x16.txt"))
    (loop for line = (read-line stream nil)
          while line collect line)))

(cl-weave:describe "Memory presentation"
  (cl-weave:it "declares the Memory widget through define-widget"
    (cl-weave:expect
     (macroexpand-1
      '(ntop.ui:define-widget :memory
         (target rectangle state config series)
         :rendered))
     :to-equal
     '(defmethod ntop.ui:draw-widget
          ((ntop.ui::widget-key (eql :memory))
           target rectangle state config series)
        (declare (ignore ntop.ui::widget-key))
        :rendered)))

  (cl-weave:it "formats RAM, swap, and enabled cache series with shared colors"
    (let ((series (%f2-memory-fixture-series)))
      (multiple-value-bind (chart-series maximum)
          (ntop.memory.presentation::%memory-chart-series
           series "default" t)
        (cl-weave:expect (mapcar #'ntop.ui:chart-series-name chart-series)
                         :to-equal
                         '("RAM: 75%   6.0GiB/8.0GiB"
                           "SWP: 50%   1.0GiB/2.0GiB"
                           "CACHE: 25%   2.0GiB/8.0GiB"))
        (cl-weave:expect maximum :to-be (float (* 8 1024 1024 1024) 1d0))
        (cl-weave:expect
         (mapcar #'ntop.ui:chart-series-style chart-series)
         :to-equal
         (mapcar (lambda (key)
                   (ntop.ui:presentation-theme-style "default" key))
                 '(:ram :swap :cache))))))

  (cl-weave:it-each
   ((0 1024 "RAM:  0%   0.0KiB/1.0KiB")
    (512 1024 "RAM: 50%   0.5KiB/1.0KiB")
    (1024 1024 "RAM:100%   1.0KiB/1.0KiB")
    (1099511627776 1099511627776
     "RAM:100%   1.0TiB/1.0TiB"))
   "formats a Memory chart legend for ~A of ~A bytes"
   (used total expected)
   (multiple-value-bind (chart-series ignored-maximum)
       (ntop.memory.presentation::%memory-chart-series
        (list :ram-samples (%f2-memory-history (list 1))
              :ram-used-bytes used
              :ram-total-bytes total)
        "default"
        nil)
     (declare (ignore ignored-maximum))
     (cl-weave:expect (mapcar #'ntop.ui:chart-series-name chart-series)
                      :to-equal
                      (list expected))))

  (cl-weave:it "omits zero-total and disabled cache series"
    (let ((series (%f2-memory-fixture-series)))
      (setf (getf series :cache-total-bytes) 0)
      (multiple-value-bind (chart-series ignored-maximum)
          (ntop.memory.presentation::%memory-chart-series series "default" t)
        (declare (ignore ignored-maximum))
        (cl-weave:expect (mapcar #'ntop.ui:chart-series-name chart-series)
                         :to-equal
                         '("RAM: 75%   6.0GiB/8.0GiB"
                           "SWP: 50%   1.0GiB/2.0GiB")))
      (setf (getf series :cache-total-bytes) (* 8 1024 1024 1024))
      (multiple-value-bind (chart-series ignored-maximum)
          (ntop.memory.presentation::%memory-chart-series series "default" nil)
        (declare (ignore ignored-maximum))
        (cl-weave:expect (mapcar #'ntop.ui:chart-series-name chart-series)
                         :to-equal
                         '("RAM: 75%   6.0GiB/8.0GiB"
                           "SWP: 50%   1.0GiB/2.0GiB")))))

  (cl-weave:it "omits invalid RAM series and uses a safe empty axis"
    (let* ((samples (%f2-memory-history '(1)))
           (invalid-series
             (list (list :ram-samples :invalid
                         :ram-used-bytes 1 :ram-total-bytes 2)
                   (list :ram-samples #()
                         :ram-used-bytes 1 :ram-total-bytes 2)
                   (list :ram-samples samples
                         :ram-used-bytes nil :ram-total-bytes 2)
                   (list :ram-samples samples
                         :ram-used-bytes -1 :ram-total-bytes 2)
                   (list :ram-samples samples
                         :ram-used-bytes 1 :ram-total-bytes nil)
                   (list :ram-samples samples
                         :ram-used-bytes 1 :ram-total-bytes -1)
                   (list :ram-samples samples
                         :ram-used-bytes 1 :ram-total-bytes 0))))
      (cl-weave:expect
       (mapcar
        (lambda (series)
          (multiple-value-bind (chart-series maximum)
              (ntop.memory.presentation::%memory-chart-series
               series "default" nil)
            (list chart-series maximum)))
        invalid-series)
       :to-equal
       (make-list 7 :initial-element (list nil 1d0)))))

  (cl-weave:it "uses configured legend placement and three-quarter constraints"
    (let* ((config
             (ntop.shared.application:make-application-config
              :memory-legend-position :bottom-left
              :dot-marker-p t))
           (chart-config
             (ntop.memory.presentation::%memory-chart-config
              "default" config 0d0 t)))
      (cl-weave:expect (ntop.ui:chart-config-legend-position chart-config)
                       :to-be :bottom-left)
      (cl-weave:expect (ntop.ui:chart-config-legend-width-ratio chart-config)
                       :to-be 3/4)
      (cl-weave:expect (ntop.ui:chart-config-legend-height-ratio chart-config)
                       :to-be 3/4)
      (cl-weave:expect (ntop.ui:chart-config-dot-marker-p chart-config)
                       :to-be t)
      (cl-weave:expect (ntop.ui:chart-config-y-max chart-config)
                       :to-be 1d0)
      (cl-weave:expect (ntop.ui:chart-config-y-labels chart-config)
                       :to-equal (list "  0%" "100%"))
      (cl-weave:expect (ntop.ui:chart-config-x-labels chart-config)
                       :to-equal (list "60s" "0s"))))

  (cl-weave:it "maps the none legend option to a hidden chart legend"
    (let* ((config
             (ntop.shared.application:make-application-config
              :memory-legend-position :none))
           (chart-config
             (ntop.memory.presentation::%memory-chart-config
              "default" config 8192d0)))
      (cl-weave:expect (ntop.ui:chart-config-legend-position chart-config)
                       :to-be nil)))

  (cl-weave:it "draws the Memory title only inside a positive rectangle"
    (let ((screen (cl-tty-kit:make-screen 12 3)))
      (ntop.memory.presentation::%draw-memory-title
       screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 1)
       "default")
      (ntop.memory.presentation::%draw-memory-title
       screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 8 :height 0)
       "default")
      (cl-weave:expect
       (search "Memory" (cl-tty-kit:screen-to-string screen))
       :to-be nil)
      (ntop.memory.presentation::%draw-memory-title
       screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 8 :height 1)
       "default")
      (cl-weave:expect
       (not (null (search "Memory" (cl-tty-kit:screen-to-string screen))))
       :to-be t)))

  (cl-weave:it "skips a Memory chart with no drawable rows"
    (let* ((screen (cl-tty-kit:make-screen 40 16))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 0))
           (config (ntop.shared.application:make-application-config)))
      (ntop.ui:draw-widget :memory screen rectangle nil config
                           (%f2-memory-fixture-series))
      (cl-weave:expect
       (search "Memory" (cl-tty-kit:screen-to-string screen))
       :to-be nil)))

  (cl-weave:it "skips RAM gauges with invalid memory values"
    (let ((screen (cl-tty-kit:make-screen 32 1))
          (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 32 :height 1)))
      (dolist (values '((nil 8) (-1 8) (8 nil) (8 -1)))
        (ntop.memory.presentation::%draw-memory-gauge
         screen rectangle
         (list :ram-used-bytes (first values)
               :ram-total-bytes (second values))
         "default"))
      (cl-weave:expect (search "RAM" (cl-tty-kit:screen-to-string screen))
                       :to-be nil)))

  (cl-weave:it "hides cache history when cache collection is disabled"
    (let* ((screen (cl-tty-kit:make-screen 80 12))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 12))
           (config (ntop.shared.application:make-application-config)))
      (ntop.ui:draw-widget :memory screen rectangle nil config
                           (%f2-memory-fixture-series))
      (cl-weave:expect
       (search "CACHE" (cl-tty-kit:screen-to-string screen))
       :to-be nil)))

  (cl-weave:it "ignores ARC history input in the Memory chart"
    (let* ((series (%f2-memory-fixture-series))
           (gib (* 1024 1024 1024))
           (series-with-arc
             (append series
                     (list :arc-samples
                           (%f2-memory-history (list gib (* 2 gib)))
                           :arc-used-bytes (* 2 gib)
                           :arc-total-bytes (* 8 gib))))
           (screen (cl-tty-kit:make-screen 40 16))
           (arc-screen (cl-tty-kit:make-screen 40 16))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 16))
           (config
             (ntop.shared.application:make-application-config
              :enable-cache-memory-p t)))
      (ntop.ui:draw-widget :memory screen rectangle nil config series)
      (ntop.ui:draw-widget :memory arc-screen rectangle nil config series-with-arc)
      (cl-weave:expect
       (cl-tty-kit:screen-to-string arc-screen)
       :to-equal
       (cl-tty-kit:screen-to-string screen))
      (cl-weave:expect
       (search "ARC" (cl-tty-kit:screen-to-string arc-screen))
       :to-be nil)
      (multiple-value-bind (normal-chart-series ignored-maximum)
          (ntop.memory.presentation::%memory-chart-series series "default" t)
        (declare (ignore ignored-maximum))
        (multiple-value-bind (arc-chart-series ignored-arc-maximum)
            (ntop.memory.presentation::%memory-chart-series
             series-with-arc "default" t)
          (declare (ignore ignored-arc-maximum))
          (cl-weave:expect
           (mapcar (lambda (item) (ntop.ui:chart-series-name item))
                   arc-chart-series)
           :to-equal
           (mapcar (lambda (item) (ntop.ui:chart-series-name item))
                   normal-chart-series))
          (cl-weave:expect
           (some (lambda (item)
                   (search "ARC" (ntop.ui:chart-series-name item)))
                 arc-chart-series)
           :to-be nil)))))

  (cl-weave:it "renders a fixture-data graph to the 40 by 16 screen golden"
    (let* ((screen (cl-tty-kit:make-screen 40 16))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 16))
           (config
             (ntop.shared.application:make-application-config
              :theme :default
              :enable-cache-memory-p t)))
      (ntop.ui:draw-widget :memory screen rectangle nil config
                           (%f2-memory-fixture-series))
      (let ((rows (%f2-memory-screen-lines screen))
            (golden-lines (%f2-memory-screen-golden-lines)))
        (cl-weave:expect (length golden-lines) :to-be 16)
        (cl-weave:expect
         (not (null (some (lambda (line) (not (string= line ""))) golden-lines)))
         :to-be t)
        (cl-weave:expect (every (lambda (line) (<= (length line) 40)) golden-lines)
                         :to-be t)
        (cl-weave:expect rows :to-equal golden-lines))))

  (cl-weave:it "draws a basic RAM gauge with the shared memory fraction label"
    (let* ((screen (cl-tty-kit:make-screen 32 1))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 32 :height 1))
           (config
             (ntop.shared.application:make-application-config :basic-p t))
           (series (%f2-memory-fixture-series)))
      (ntop.ui:draw-widget :memory screen rectangle nil config series)
      (progn
            (cl-weave:expect (char (cl-tty-kit:screen-row-string screen 0) 0)
                             :to-be #\Space)
            (cl-weave:expect
             (subseq (cl-tty-kit:screen-row-string screen 0) 1 4)
             :to-equal "RAM"))
      (cl-weave:expect
       (not (null (search "6.0GiB/8.0GiB"
                          (cl-tty-kit:screen-to-string screen))))
       :to-be t)))
  (cl-weave:it "appends GPU memory histories after RAM, swap, and cache"
    (let* ((gib (* 1024 1024 1024))
           (gpu-series
             (list
              (ntop.ui:make-chart-series
               :name "GPU zero"
               :style :gpu-zero
               :samples (%f2-memory-history (list (* 2 gib) (* 4 gib))))
              (ntop.ui:make-chart-series
               :name "GPU one"
               :style :gpu-one
               :samples (%f2-memory-history (list (* 3 gib) (* 5 gib))))))
           (series
             (append (%f2-memory-fixture-series)
                     (list :gpu-memory-series gpu-series
                           :gpu-memory-total-bytes (* 16 gib)))))
      (multiple-value-bind (chart-series maximum)
          (ntop.memory.presentation::%memory-chart-series
           series "default" t)
        (cl-weave:expect
         (loop for item in chart-series
               collect (ntop.ui:chart-series-name item))
         :to-equal
         (list "RAM: 75%   6.0GiB/8.0GiB"
               "SWP: 50%   1.0GiB/2.0GiB"
               "CACHE: 25%   2.0GiB/8.0GiB"
               "GPU zero"
               "GPU one"))
        (cl-weave:expect
         (loop for item in chart-series
               collect (ntop.ui:chart-series-style item))
         :to-equal
         (list (ntop.ui:presentation-theme-style "default" :ram)
               (ntop.ui:presentation-theme-style "default" :swap)
               (ntop.ui:presentation-theme-style "default" :cache)
               :gpu-zero
               :gpu-one))
        (cl-weave:expect maximum :to-be (float (* 16 gib) 1d0))
        (cl-weave:expect
         (loop for item in (nthcdr 3 chart-series)
               collect
               (loop for sample across (ntop.ui:chart-series-samples item)
                     collect
                     (ntop.shared.domain:history-sample-value sample)))
         :to-equal
         (list (list (float (* 2 gib) 1d0)
                    (float (* 4 gib) 1d0))
               (list (float (* 3 gib) 1d0)
                     (float (* 5 gib) 1d0)))))))
  (cl-weave:it "omits empty GPU histories without requiring or using capacity"
    (let* ((gib (* 1024 1024 1024))
           (empty-series
             (ntop.ui:make-chart-series
              :name "Empty GPU"
              :style :empty-gpu
              :samples nil))
           (base-series (%f2-memory-fixture-series)))
      (dolist (total (list nil (* 64 gib)))
        (let ((series
                (append base-series
                        (list :gpu-memory-series (list empty-series)
                              :gpu-memory-total-bytes total))))
          (multiple-value-bind (chart-series maximum)
              (ntop.memory.presentation::%memory-chart-series
               series "default" nil)
            (cl-weave:expect
             (loop for item in chart-series
                   collect (ntop.ui:chart-series-name item))
             :to-equal
             (list "RAM: 75%   6.0GiB/8.0GiB"
                   "SWP: 50%   1.0GiB/2.0GiB"))
            (cl-weave:expect maximum :to-be (float (* 8 gib) 1d0)))))))
  (cl-weave:it "requires a positive integer total for sampled GPU histories"
    (let* ((gpu-series
             (ntop.ui:make-chart-series
              :name "GPU"
              :style :gpu
              :samples (%f2-memory-history (list 256))))
           (invalid-totals (list nil 0 -1 1.5 "16")))
      (cl-weave:expect
       (loop for total in invalid-totals
             always
             (handler-case
                 (progn
                   (ntop.memory.presentation::%memory-chart-series
                    (list :gpu-memory-series (list gpu-series)
                          :gpu-memory-total-bytes total)
                    "default"
                    nil)
                   nil)
               (error () t)))
       :to-be t)))
  (cl-weave:it "keeps the no-GPU call shape and axis maximum"
    (let* ((gib (* 1024 1024 1024))
           (series (%f2-memory-fixture-series)))
      (multiple-value-bind (chart-series maximum)
          (ntop.memory.presentation::%memory-chart-series
           series "default" t)
        (cl-weave:expect
         (loop for item in chart-series
               collect (ntop.ui:chart-series-name item))
         :to-equal
         (list "RAM: 75%   6.0GiB/8.0GiB"
               "SWP: 50%   1.0GiB/2.0GiB"
               "CACHE: 25%   2.0GiB/8.0GiB"))
        (cl-weave:expect maximum :to-be (float (* 8 gib) 1d0))))))

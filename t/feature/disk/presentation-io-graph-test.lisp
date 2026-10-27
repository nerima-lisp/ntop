(in-package #:ntop.test)

(defun %f5-disk-graph-history (name values)
  (let ((history
         (ntop.shared.domain:make-history-store 1
                                                :retention-seconds
                                                5d0
                                                :sample-interval-seconds
                                                1d0
                                                :initial-window-seconds
                                                5d0
                                                :minimum-window-seconds
                                                1d0)))
    (loop for value in values
          for index from 0
          do (ntop.shared.domain:history-append history
                                                0
                                                (ntop.shared.domain:make-sample-record
                                                 :sampled-at
                                                 (float (1+ index) 1d0))
                                                value))
    (ntop.ui:make-chart-series :name
                               name
                               :samples
                               (ntop.shared.domain:history-slice history 0))))

(defun %f5-disk-graph-screen-lines (screen)
  (loop for row below (cl-tty-kit:screen-height screen)
        collect (string-right-trim '(#\Space)
                                   (cl-tty-kit:screen-row-string screen row))))

(defun %f5-disk-graph-golden-lines ()
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop/test"
                                      "t/feature/disk/goldens/presentation-io-graph-40x16.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun %f5-disk-graph-fixture-series ()
  (list :read
        (list (%f5-disk-graph-history "disk2" '(0 512 1024))
              (%f5-disk-graph-history "disk1" '(0 1024 2048)))
        :write
        (list (%f5-disk-graph-history "disk2" '(0 0 0))
              (%f5-disk-graph-history "disk1" '(0 2048 3072)))))

(cl-weave:describe "Disk I/O graph presentation"
                   (cl-weave:it
                    "registers the disk I/O graph widget with its chart configuration"
                    (cl-weave:expect
                     (macroexpand-1
                      '(ntop.ui:define-widget :disk-io-graph
                                              (target rectangle
                                                      state
                                                      config
                                                      series)
                                              :rendered))
                     :to-equal
                     '(defmethod ntop.ui:draw-widget ((ntop.ui::widget-key
                                                       (eql :disk-io-graph))
                                                      target
                                                      rectangle
                                                      state
                                                      config
                                                      series)
                        (declare (ignore ntop.ui::widget-key))
                        :rendered)))
                   (cl-weave:it
                    "sorts disk names and pairs read/write rates with their samples"
                    (let* ((graph-series
                            (ntop.disk.presentation::%disk-io-chart-series
                             (%f5-disk-graph-fixture-series)
                             "default"))
                           (names
                            (mapcar #'ntop.ui:chart-series-name graph-series)))
                      (cl-weave:expect
                       (mapcar
                        (lambda (name)
                          (subseq name 0 5))
                        names)
                       :to-equal
                       '("disk1" "disk1" "disk2" "disk2"))
                      (cl-weave:expect
                       (mapcar
                        (lambda (name)
                          (subseq name 6 8))
                        names)
                       :to-equal
                       '("R:" "W:" "R:" "W:"))
                      (cl-weave:expect
                       (mapcar
                        (lambda (item)
                          (ntop.shared.domain:history-sample-value
                           (aref (ntop.ui:chart-series-samples item) 2)))
                        graph-series)
                       :to-equal
                       '(2048d0 3072d0 1024d0 0d0))
                      (cl-weave:expect
                       (ntop.ui:chart-series-style (first graph-series))
                       :to-equal
                       (first
                        (ntop.ui:presentation-theme-style "default"
                                                          :disk-io-read-colors)))
                      (cl-weave:expect
                       (ntop.ui:chart-series-style (second graph-series))
                       :to-equal
                       (first
                        (ntop.ui:presentation-theme-style "default"
                                                          :disk-io-write-colors)))))
                   (cl-weave:it
                    "uses the shared axis labels and btm legend constraints"
                    (let* ((series
                            (ntop.disk.presentation::%disk-io-chart-series
                             (%f5-disk-graph-fixture-series)
                             "default"))
                           (config
                            (ntop.disk.presentation::%disk-io-chart-config
                             "default"
                             (ntop.shared.application:make-application-config)
                             series
                             t)))
                      (cl-weave:expect (ntop.ui:chart-config-y-min config)
                                       :to-be
                                       0d0)
                      (cl-weave:expect (ntop.ui:chart-config-y-max config)
                                       :to-be
                                       4608d0)
                      (cl-weave:expect
                       (list (ntop.ui:chart-config-x-start config)
                             (ntop.ui:chart-config-x-end config)
                             (ntop.ui:chart-config-x-labels config)
                             (ntop.ui:chart-config-y-labels config))
                       :to-equal
                       (list -57d0
                             3d0
                             (list "60s" "0s")
                             (list "  0KiB/s"
                                   "     1.5"
                                   "     3.0"
                                   "     4.5")))
                      (cl-weave:expect
                       (ntop.ui:chart-config-legend-width-ratio config)
                       :to-be
                       9/10)
                      (cl-weave:expect
                       (ntop.ui:chart-config-legend-height-ratio config)
                       :to-be
                       1/2)))
                   (cl-weave:it
                    "renders fixture histories to the 40 by 16 screen golden"
                    (let* ((screen (cl-tty-kit:make-screen 40 16))
                           (rectangle
                            (ntop.ui:make-rectangle :x
                                                    0
                                                    :y
                                                    0
                                                    :width
                                                    40
                                                    :height
                                                    16))
                           (config
                            (ntop.shared.application:make-application-config
                             :theme
                             :default)))
                      (ntop.ui:draw-widget :disk-io-graph
                                           screen
                                           rectangle
                                           nil
                                           config
                                           (%f5-disk-graph-fixture-series))
                      (cl-weave:expect (%f5-disk-graph-screen-lines screen)
                                       :to-equal
                                       (%f5-disk-graph-golden-lines))))
                   (cl-weave:it
                    "draws an empty graph without requiring disk series"
                    (let* ((screen (cl-tty-kit:make-screen 20 6))
                           (rectangle
                            (ntop.ui:make-rectangle :x
                                                    0
                                                    :y
                                                    0
                                                    :width
                                                    20
                                                    :height
                                                    6))
                           (config
                            (ntop.shared.application:make-application-config)))
                      (ntop.ui:draw-widget :disk-io-graph
                                           screen
                                           rectangle
                                           nil
                                           config
                                           nil)
                      (cl-weave:expect
                       (not
                        (null
                         (search "Disk I/O"
                                 (cl-tty-kit:screen-to-string screen))))
                       :to-be
                       t)))
                   (cl-weave:it
                    "labels an empty disk history and leaves an empty axis at zero"
                    (let* ((empty-series
                            (ntop.ui:make-chart-series :name
                                                       "disk8"
                                                       :samples
                                                       #()))
                           (series
                            (ntop.disk.presentation::%disk-io-chart-series
                             (list :read (list empty-series) :write nil)
                             "default")))
                      (cl-weave:expect (length series) :to-be 1)
                      (cl-weave:expect
                       (search "N/A" (ntop.ui:chart-series-name (first series)))
                       :to-be-truthy)
                      (cl-weave:expect
                       (ntop.ui:chart-series-maximum-value series)
                       :to-be
                       0)
                      (cl-weave:expect
                       (ntop.disk.presentation::%disk-io-chart-series
                        (list :read nil :write nil)
                        "default")
                       :to-equal
                       nil)))
                   (cl-weave:it "keeps series when the theme has no I/O colors"
                                (let* ((source
                                        (%f5-disk-graph-history "disk8"
                                                                '(0 512)))
                                       (series nil))
                                  (cl-weave:with-mocked-functions
                                   (((symbol-function
                                      'ntop.ui:presentation-theme-style)
                                     (lambda (theme style)
                                       (declare (ignore theme style))
                                       nil)))
                                   (setf series (ntop.disk.presentation::%disk-io-series-pair
                                                 (list source)
                                                 nil
                                                 "disk8"
                                                 0
                                                 5
                                                 "default")))
                                  (cl-weave:expect (length series) :to-be 1)
                                  (cl-weave:expect
                                   (ntop.ui:chart-series-style (first series))
                                   :to-be-null)
                                  (cl-weave:expect
                                   (ntop.disk.presentation::%disk-io-series-pair
                                    nil
                                    (list source)
                                    "disk8"
                                    0
                                    5
                                    "default")
                                   :to-be-truthy)))
                   (cl-weave:it
                    "skips drawing the title and chart for zero-sized rectangles"
                    (let* ((screen (cl-tty-kit:make-screen 20 6))
                           (config
                            (ntop.shared.application:make-application-config)))
                      (ntop.disk.presentation::%draw-disk-io-title screen
                                                                   (ntop.ui:make-rectangle
                                                                    :x
                                                                    0
                                                                    :y
                                                                    0
                                                                    :width
                                                                    0
                                                                    :height
                                                                    1)
                                                                   "default")
                      (ntop.ui:draw-widget :disk-io-graph
                                           screen
                                           (ntop.ui:make-rectangle :x
                                                                   0
                                                                   :y
                                                                   0
                                                                   :width
                                                                   20
                                                                   :height
                                                                   0)
                                           nil
                                           config
                                           nil)
                      (cl-weave:expect
                       (search "Disk I/O" (cl-tty-kit:screen-to-string screen))
                       :to-be-null)))
                   (cl-weave:it
 "hides time labels at tall heights when configured to hide time"
 (let* ((screen (cl-tty-kit:make-screen 40 8))
        (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 8))
        (config
         (ntop.shared.application:make-application-config
          :hide-time-p t
          :time-window-seconds 60)))
   (ntop.ui:draw-widget
    :disk-io-graph screen rectangle nil config
    (%f5-disk-graph-fixture-series))
   (let ((snapshot (cl-tty-kit:screen-to-string screen)))
     (cl-weave:expect (not (null (search "Disk I/O" snapshot))) :to-be t)
     (cl-weave:expect (search "60s" snapshot) :to-be-null)
     (cl-weave:expect (search "0s" snapshot) :to-be-null)))))

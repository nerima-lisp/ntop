(in-package #:ntop.test)

(defun %f1-cpu-fixture-data ()
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop"
                                      "t/feature/cpu/fixtures/presentation.lisp"))
    (read stream)))

(defun %f1-cpu-history (values)
  (let ((history
         (ntop.shared.domain:make-history-store 1
                                                :retention-seconds
                                                4d0
                                                :sample-interval-seconds
                                                1d0
                                                :initial-window-seconds
                                                4d0
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
    (ntop.shared.domain:history-slice history 0)))

(defun %f1-cpu-fixture-series ()
  (let* ((fixture (%f1-cpu-fixture-data))
         (sample
          (ntop.cpu.domain:make-cpu-sample :sampled-at
                                           (getf fixture :sampled-at)
                                           :core-values
                                           (loop for core in (getf fixture
                                                                   :cores)
                                                 collect (ntop.cpu.domain:make-cpu-core-value
                                                          :core-index
                                                          (getf core :index)
                                                          :usage-percent
                                                          (getf core
                                                                :usage-percent)
                                                          :frequency-mhz
                                                          (getf core
                                                                :frequency-mhz)))
                                           :average-usage-percent
                                           (getf fixture :average-usage-percent)
                                           :load-average-1
                                           (getf fixture :load-average-1)
                                           :load-average-5
                                           (getf fixture :load-average-5)
                                           :load-average-15
                                           (getf fixture :load-average-15)))
         (usage
          (loop for (name values) in (getf fixture :history)
                collect (ntop.ui:make-chart-series :name
                                                   name
                                                   :samples
                                                   (%f1-cpu-history values)))))
    (list :sample sample :usage usage)))

(defun %f1-cpu-row (rows kind index)
  (find-if
   (lambda (row)
     (and (eq kind (getf row :kind))
          (or (not (eq kind :cpu)) (eql index (getf row :index)))))
   rows))

(defun %f1-cpu-screen-rows (screen)
  (loop for row below (cl-tty-kit:screen-height screen)
        collect (string-right-trim '(#\Space)
                                   (cl-tty-kit:screen-row-string screen row))))

(defun %f1-cpu-screen-golden-lines ()
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop"
                                      "t/feature/cpu/goldens/presentation-80x16.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(cl-weave:describe "CPU presentation"
  (cl-weave:it "declares the CPU widget through define-widget"
    (cl-weave:expect
     (macroexpand-1
      '(ntop.ui:define-widget :cpu
         (target rectangle state config series)
         :rendered))
     :to-equal
     '(defmethod ntop.ui:draw-widget
          ((ntop.ui::widget-key (eql :cpu))
           target rectangle state config series)
        (declare (ignore ntop.ui::widget-key))
        :rendered)))

  ;; bottom 0.14.9 src/widgets/cpu_graph.rs:34-97,118-125.
  (cl-weave:it "matches the All, AVG, core-label and usage cells from the btm rule table"
    (let* ((series (%f1-cpu-fixture-series))
           (sample (getf series :sample))
           (config
             (ntop.shared.application:make-application-config :expanded-p t)))
      (dolist (case '((:all nil 4 "All" "" "")
                      (:average nil 4 "AVG" "54%" "N/A")
                      (:cpu 0 4 "0" "32%" "2200")
                      (:cpu 0 5 "CPU0" "32%" "2200")
                      (:cpu 1 4 "1" "69%" "N/A")))
        (destructuring-bind (kind index width label usage frequency) case
          (let* ((rows
                   (ntop.cpu.presentation::%cpu-legend-rows
                    sample config width))
                 (row (%f1-cpu-row rows kind index)))
            (cl-weave:expect (getf row :label) :to-equal label)
            (cl-weave:expect (getf row :usage) :to-equal usage)
            (cl-weave:expect (getf row :frequency) :to-equal frequency))))))

  ;; bottom 0.14.9 src/collection/cpu/sysinfo.rs:6-40.
  (cl-weave:it "omits AVG rows when hide-average-cpu is enabled"
    (let* ((series (%f1-cpu-fixture-series))
           (config
             (ntop.shared.application:make-application-config
              :hide-average-cpu-p t))
           (rows
             (ntop.cpu.presentation::%cpu-legend-rows
              (getf series :sample) config 5)))
      (cl-weave:expect (mapcar (lambda (row) (getf row :label)) rows)
                       :to-equal '("All" "CPU0" "CPU1" "CPU2" "CPU3"))
      (cl-weave:expect
       (not (find :average rows :key (lambda (row) (getf row :kind))))
       :to-be t)))

  ;; bottom 0.14.9 src/components/time_series/percent.rs:30-37.
  (cl-weave:it "uses the 60-second CPU window and percentage axis"
    (let* ((fixture (%f1-cpu-fixture-series))
           (config
             (ntop.shared.application:make-application-config :dot-marker-p t))
           (chart-config
             (ntop.cpu.presentation::%cpu-chart-config
              "default" config (getf fixture :usage) t)))
      (cl-weave:expect (ntop.ui:chart-config-x-start chart-config)
                       :to-be -57d0)
      (cl-weave:expect (ntop.ui:chart-config-x-end chart-config)
                       :to-be 3d0)
      (cl-weave:expect (ntop.ui:chart-config-y-min chart-config) :to-be 0d0)
      (cl-weave:expect (ntop.ui:chart-config-y-max chart-config) :to-be 100.5d0)
      (cl-weave:expect (ntop.ui:chart-config-y-labels chart-config)
                       :to-equal (list "  0%" "100%"))
      (cl-weave:expect (ntop.ui:chart-config-x-labels chart-config)
                       :to-equal (list "60s" "0s"))
      (cl-weave:expect (ntop.ui:chart-config-legend-position chart-config)
                       :to-be nil)
      (cl-weave:expect (ntop.ui:chart-config-dot-marker-p chart-config)
                       :to-be t)
      (cl-weave:expect
       (ntop.cpu.presentation::%cpu-title (getf fixture :sample))
       :to-equal " CPU ─ 1.25 0.88 0.55 ")))

  ;; bottom 0.14.9 src/widgets/cpu_graph.rs:26-41,118-125,146-164.
  (cl-weave:it "matches BTM CPU legend widths and preserves N/A frequency"
    ;; BTM 0.14.9 src/widgets/cpu_graph.rs:25-31,118-125,146-167;
    ;; src/canvas/components/data_table/column.rs:182-255;
    ;; src/canvas/components/data_table/draw.rs:224-261.
    (let* ((series (%f1-cpu-fixture-series))
           (sample (getf series :sample)))
      (dolist (case '((nil 5 (:cpu) (4) ("CPU") ("All" "AVG" "0" "1" "2" "3"))
                      (nil 14 (:cpu :use) (6 6) ("CPU" "Use")
                       ("All" "AVG" "CPU0" "CPU1" "CPU2" "CPU3"))
                      (nil 20 (:cpu :use) (9 9) ("CPU" "Use")
                       ("All" "AVG" "CPU0" "CPU1" "CPU2" "CPU3"))
                      (t 20 (:cpu :use :mhz) (6 5 6) ("CPU" "Use" "MHz")
                       ("All" "AVG" "CPU0" "CPU1" "CPU2" "CPU3"))))
        (destructuring-bind (expanded-p width expected-columns expected-widths
                                        expected-headers expected-labels)
            case
          (let* ((config
                   (ntop.shared.application:make-application-config
                    :expanded-p expanded-p))
                 (rows
                   (ntop.cpu.presentation::%cpu-legend-rows sample config 5)))
            (multiple-value-bind (columns visible-rows)
                (ntop.cpu.presentation::%cpu-legend-view sample config width)
              (cl-weave:expect (mapcar #'ntop.ui:table-column-id columns)
                               :to-equal
                               expected-columns)
              (cl-weave:expect (mapcar #'ntop.ui:table-column-width columns)
                               :to-equal
                               expected-widths)
              (cl-weave:expect (mapcar #'ntop.ui:table-column-header columns)
                               :to-equal
                               expected-headers)
              (cl-weave:expect (mapcar (lambda (row) (getf row :label))
                                       visible-rows)
                               :to-equal
                               expected-labels)
              (cl-weave:expect
               (getf (%f1-cpu-row visible-rows :cpu 1) :frequency)
               :to-equal
               "N/A")
              (cl-weave:expect
               (getf (%f1-cpu-row rows :cpu 1) :usage)
               :to-equal
               "69%")))))))

  ;; bottom 0.14.9 src/canvas/widgets/cpu_graph.rs:118-169.
  (cl-weave:it "shows all, average, or the selected core history"
    (let* ((series (%f1-cpu-fixture-series))
           (sample (getf series :sample))
           (config (ntop.shared.application:make-application-config))
           (rows
             (ntop.cpu.presentation::%cpu-legend-rows sample config 5)))
      (dolist (case '((0 ("CPU3" "CPU2" "CPU1" "CPU0" "AVG"))
                      (1 ("AVG"))
                      (2 ("CPU0"))
                      (3 ("CPU1"))))
        (destructuring-bind (selected-index expected-names) case
          (let ((selected
                  (ntop.cpu.presentation::%cpu-selected-chart-series
                   (getf series :usage)
                   rows
                   (ntop.ui:make-table-state :selected-index selected-index)
                   "default")))
            (cl-weave:expect (mapcar #'ntop.ui:chart-series-name selected)
                             :to-equal
                             expected-names))))))

  ;; bottom 0.14.9 src/canvas/widgets/cpu_basic.rs:48-79,81-145,159-176.
  (cl-weave:it "places AVG first in the four-column basic CPU gauges"
    (let* ((screen (cl-tty-kit:make-screen 64 3))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 64 :height 3))
           (config
             (ntop.shared.application:make-application-config :basic-p t))
           (series (%f1-cpu-fixture-series))
           (sample (getf series :sample)))
      (ntop.ui:draw-widget :cpu screen rectangle nil config series)
      (let ((first-row (cl-tty-kit:screen-row-string screen 0))
            (second-row (cl-tty-kit:screen-row-string screen 1))
            (last-row (cl-tty-kit:screen-row-string screen 2)))
        (cl-weave:expect
         (loop for x in (list 1 17 33 49)
               collect (subseq first-row x (+ x 3)))
         :to-equal (list "AVG" "1  " "2  " "3  "))
        (cl-weave:expect (subseq second-row 1 4) :to-equal "0  ")
        (cl-weave:expect (not (null (search "54%" first-row))) :to-be t)
        (cl-weave:expect (search "AVG" last-row) :to-equal nil)
        (cl-weave:expect
         (every (lambda (character) (char= character (code-char 32)))
                last-row)
         :to-be t))
      (cl-weave:expect
       (ntop.cpu.domain:cpu-core-value-frequency-mhz
        (second (ntop.cpu.domain:cpu-sample-core-values sample)))
       :to-be
       nil)))

  (cl-weave:it "renders fixture-backed CPU data to the 80 by 16 screen golden"
    (let* ((screen (cl-tty-kit:make-screen 80 16))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 16))
           (config
             (ntop.shared.application:make-application-config
              :theme :default
              :expanded-p t))
           (series (%f1-cpu-fixture-series)))
      (ntop.ui:draw-widget :cpu screen rectangle
                           (ntop.ui:make-table-state)
                           config
                           series)
      (let ((rows (%f1-cpu-screen-rows screen))
            (golden-lines (%f1-cpu-screen-golden-lines)))
        (cl-weave:expect (length golden-lines) :to-be 16)
        (cl-weave:expect
         (not (null (some (lambda (line) (plusp (length line))) golden-lines)))
         :to-be t)
        (cl-weave:expect (every (lambda (line) (<= (length line) 80)) golden-lines)
                         :to-be t)
        (cl-weave:expect rows :to-equal golden-lines)))))

(cl-weave:describe "CPU presentation edge cases"
  (cl-weave:it "classifies history names and selects only the requested CPU row"
    (dolist (case (list (list "AVG" :average nil)
                        (list "avg" :average nil)
                        (list "cpu0" :cpu 0)
                        (list "CPU00" :cpu 0)
                        (list "CPU" nil nil)
                        (list "CPUx" nil nil)
                        (list nil nil nil)))
      (destructuring-bind (name expected-kind expected-index) case
        (multiple-value-bind (kind index)
            (ntop.cpu.presentation::%cpu-series-kind
             (ntop.ui:make-chart-series :name name))
          (cl-weave:expect kind :to-equal expected-kind)
          (cl-weave:expect index :to-equal expected-index))))
    (let* ((series (%f1-cpu-fixture-series))
           (sample (getf series :sample))
           (rows (ntop.cpu.presentation::%cpu-legend-rows
                  sample
                  (ntop.shared.application:make-application-config)
                  5))
           (colors (ntop.ui:presentation-theme-style "default" :cpu-colors))
           (explicit (ntop.ui:make-chart-series :name "other" :style :explicit))
           (unknown (ntop.ui:make-chart-series :name "other")))
      (cl-weave:expect
       (ntop.cpu.presentation::%cpu-chart-series-style explicit 0 "default")
       :to-equal :explicit)
      (cl-weave:expect
       (ntop.cpu.presentation::%cpu-chart-series-style unknown 2 "default")
       :to-equal (nth 2 colors))
      (cl-weave:expect
       (ntop.cpu.presentation::%cpu-selected-chart-series
        nil rows (ntop.ui:make-table-state) "default")
       :to-equal nil)
      (cl-weave:expect
       (mapcar (function ntop.ui:chart-series-name)
               (ntop.cpu.presentation::%cpu-selected-chart-series
                (getf series :usage)
                rows
                (ntop.ui:make-table-state :selected-index -2)
                "default"))
       :to-equal (list "CPU3" "CPU2" "CPU1" "CPU0" "AVG"))
      (cl-weave:expect
       (mapcar (function ntop.ui:chart-series-name)
               (ntop.cpu.presentation::%cpu-selected-chart-series
                (getf series :usage)
                rows
                (ntop.ui:make-table-state :selected-index 100)
                "default"))
       :to-equal (list "CPU3")))))

(cl-weave:describe "CPU presentation pending and unavailable data"
  (cl-weave:it "distinguishes pending collection from unavailable values"
    (let* ((config (ntop.shared.application:make-application-config))
           (rows (ntop.cpu.presentation::%cpu-legend-rows nil config 5))
           (partial
             (ntop.cpu.domain:make-cpu-sample :sampled-at 1d0 :load-average-1 1.25d0))
           (partial-rows
             (ntop.cpu.presentation::%cpu-legend-rows partial config 5)))
      (cl-weave:expect (ntop.cpu.presentation::%cpu-title nil)
                       :to-equal " CPU ─ No data ")
      (cl-weave:expect (ntop.cpu.presentation::%cpu-title partial)
                       :to-equal " CPU ─ 1.25 N/A N/A ")
      (cl-weave:expect (getf (second rows) :usage) :to-equal "No data")
      (cl-weave:expect (getf (second partial-rows) :usage) :to-equal "N/A")
      (cl-weave:expect (ntop.cpu.presentation::%cpu-usage-text nil)
                       :to-equal "N/A")
      (cl-weave:expect (ntop.cpu.presentation::%cpu-frequency-text -1)
                       :to-equal "N/A")
      (cl-weave:expect
       (handler-case
           (progn (ntop.cpu.presentation::%cpu-sample (list :sample :invalid))
                  nil)
         (error () t))
       :to-be t)
      (let* ((sample (getf (%f1-cpu-fixture-series) :sample))
             (rows (ntop.cpu.presentation::%cpu-legend-rows sample config 5))
             (average-config
               (ntop.shared.application:make-application-config
                :default-cpu-entry :average))
             (hidden-config
               (ntop.shared.application:make-application-config
                :default-cpu-entry :average
                :hide-average-cpu-p t)))
        (cl-weave:expect
         (ntop.ui:table-state-selected-index
          (ntop.cpu.presentation::%cpu-state nil average-config rows))
         :to-be 1)
        (cl-weave:expect
         (ntop.ui:table-state-selected-index
          (ntop.cpu.presentation::%cpu-state
           nil hidden-config
           (ntop.cpu.presentation::%cpu-legend-rows sample hidden-config 5)))
         :to-be 0)
        (cl-weave:expect
         (ntop.ui:table-state-selected-index
          (ntop.cpu.presentation::%cpu-state
           (ntop.ui:make-table-state :selected-index 100) config rows))
         :to-be (1- (length rows)))
        (cl-weave:expect
         (handler-case
             (progn (ntop.cpu.presentation::%cpu-state :invalid config rows)
                    nil)
           (error () t))
         :to-be t)))))

(cl-weave:describe "CPU presentation constrained drawing"
  (cl-weave:it "draws a title and legend when the graph has one row"
    (let* ((fixture (%f1-cpu-fixture-series))
           (sample (getf fixture :sample))
           (config (ntop.shared.application:make-application-config
                    :expanded-p t))
           (rows (ntop.cpu.presentation::%cpu-legend-rows sample config 5))
           (screen (cl-tty-kit:make-screen 24 1))
           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 24 :height 1)))
      (ntop.cpu.presentation::%draw-cpu-chart
       screen rectangle (ntop.ui:make-table-state) sample config rows fixture
       "default")
      (cl-weave:expect
       (not (null (search "CPU" (cl-tty-kit:screen-to-string screen))))
       :to-be t)
      (ntop.cpu.presentation::%draw-cpu-legend
       screen rectangle (ntop.ui:make-table-state) sample config "default")
      (cl-weave:expect
       (not (null (search "All" (cl-tty-kit:screen-to-string screen))))
       :to-be t)
      (let ((empty-screen (cl-tty-kit:make-screen 8 1)))
        (ntop.cpu.presentation::%draw-cpu-title
         empty-screen
         (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 1)
         sample
         "default")
        (cl-weave:expect
         (search "CPU" (cl-tty-kit:screen-to-string empty-screen))
         :to-equal nil)))
    (let* ((screen (cl-tty-kit:make-screen 80 16))
           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 16))
           (fixture (%f1-cpu-fixture-series))
           (config (ntop.shared.application:make-application-config
                    :cpu-left-legend-p t)))
      (ntop.cpu.presentation::%draw-cpu-graph
       screen rectangle (ntop.ui:make-table-state) (getf fixture :sample)
       config fixture "default")
      (cl-weave:expect
       (not (null (search "CPU" (cl-tty-kit:screen-to-string screen))))
       :to-be t))))

(cl-weave:describe "CPU basic presentation edge cases"
  (cl-weave:it "draws empty, average-only, and unavailable-core gauges"
    (let* ((config (ntop.shared.application:make-application-config :basic-p t))
           (empty-screen (cl-tty-kit:make-screen 12 1))
           (empty-rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 12 :height 1)))
      (ntop.ui:draw-widget :cpu empty-screen empty-rectangle nil config nil)
      (cl-weave:expect
       (not (null (search "No data" (cl-tty-kit:screen-to-string empty-screen))))
       :to-be t))
    (let* ((sample (ntop.cpu.domain:make-cpu-sample :sampled-at 1d0 :average-usage-percent 25d0))
           (screen (cl-tty-kit:make-screen 24 2))
           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 24 :height 2))
           (config (ntop.shared.application:make-application-config :basic-p t)))
      (ntop.ui:draw-widget :cpu screen rectangle nil config (list :sample sample))
      (progn
       (cl-weave:expect
        (search "AVG" (cl-tty-kit:screen-row-string screen 0))
        :to-equal nil)
       (cl-weave:expect
        (subseq (cl-tty-kit:screen-row-string screen 0) 1 5)
        :to-equal " 25%")
       (cl-weave:expect
        (search "25%" (cl-tty-kit:screen-row-string screen 1))
        :to-equal nil)))
    (let* ((sample
             (ntop.cpu.domain:make-cpu-sample :sampled-at
                                              1d0
                                              :core-values
                                              (list
                                               (ntop.cpu.domain:make-cpu-core-value
                                                :core-index
                                                nil
                                                :usage-percent
                                                nil
                                                :frequency-mhz
                                                nil))))
           (gauges (ntop.cpu.presentation::%cpu-basic-gauges sample "default"))
           (gauge (first gauges))
           (screen (cl-tty-kit:make-screen 64 2))
           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 64 :height 2))
           (config (ntop.shared.application:make-application-config :basic-p t)))
      (cl-weave:expect (ntop.ui:gauge-value-start-label gauge) :to-equal "0  ")
      (cl-weave:expect (ntop.ui:gauge-value-inner-label gauge) :to-equal "N/A")
      (cl-weave:expect (ntop.ui:gauge-value-ratio gauge) :to-equal 0d0)
      (cl-weave:expect (ntop.cpu.presentation::%cpu-basic-core-label 1234)
                       :to-equal "1234")
      (ntop.ui:draw-widget :cpu screen rectangle nil config (list :sample sample))
      (cl-weave:expect
       (not (null (search "N/A" (cl-tty-kit:screen-to-string screen))))
       :to-be t)
      (let ((zero-height-screen (cl-tty-kit:make-screen 12 1)))
        (ntop.ui:draw-widget
         :cpu zero-height-screen
         (ntop.ui:make-rectangle :x 0 :y 0 :width 12 :height 0)
         nil config nil)
        (cl-weave:expect
         (search "N/A" (cl-tty-kit:screen-to-string zero-height-screen))
         :to-equal nil)))
    (let* ((screen (cl-tty-kit:make-screen 64 3))
           (fixture (%f1-cpu-fixture-series))
           (config
             (ntop.shared.application:make-application-config
              :basic-p t :hide-average-cpu-p t)))
      (ntop.ui:draw-widget
       :cpu screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 64 :height 3)
       nil config fixture)
      (cl-weave:expect
       (search "AVG" (cl-tty-kit:screen-to-string screen))
       :to-equal nil))))

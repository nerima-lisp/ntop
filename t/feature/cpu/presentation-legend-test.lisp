(in-package #:ntop.test)

(cl-weave:describe "CPU basic empty width"
  (cl-weave:it "does not draw an empty value into a zero-width area"
    (let* ((screen (cl-tty-kit:make-screen 8 1))
           (config (ntop.shared.application:make-application-config :basic-p t)))
      (ntop.ui:draw-widget
       :cpu screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 1)
       nil config nil)
      (cl-weave:expect
       (search "N/A" (cl-tty-kit:screen-to-string screen))
       :to-equal nil))))

(cl-weave:describe "CPU legend without layout"
  (cl-weave:it "keeps a one-character row label when no column fits"
    (let* ((sample (getf (%f1-cpu-fixture-series) :sample))
           (config
             (ntop.shared.application:make-application-config :expanded-p t)))
      (multiple-value-bind (columns rows)
          (ntop.cpu.presentation::%cpu-legend-view sample config 0)
        (cl-weave:expect columns :to-equal nil)
        (cl-weave:expect (getf (first rows) :label) :to-equal "All")
        (cl-weave:expect (getf (third rows) :label) :to-equal "0"))
      (let* ((screen (cl-tty-kit:make-screen 8 1))
             (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 1)))
        (ntop.cpu.presentation::%draw-cpu-legend
         screen rectangle (ntop.ui:make-table-state) sample config "default")
        (cl-weave:expect
         (search "All" (cl-tty-kit:screen-to-string screen))
         :to-equal nil)))))

(cl-weave:describe "CPU legend text and frame layout"
  (cl-weave:it "uses theme text for missing row styles and unknown roles"
    (let* ((screen (cl-tty-kit:make-screen 16 2))
           (target
             (ntop.cpu.presentation::%make-cpu-legend-target
              screen "default" nil 0 0 0)))
      (ntop.ui:draw-table-text target 0 0 "fallback" :unknown)
      (ntop.ui:draw-table-text target 0 1 "normal" :normal)
      (cl-weave:expect
       (search "fallback" (cl-tty-kit:screen-to-string screen))
       :to-be 0)
      (cl-weave:expect
       (search "normal" (cl-tty-kit:screen-to-string screen))
       :to-be 17)))
  ;; bottom 0.14.9 src/canvas/components/data_table/draw.rs:162-165,201-222,258-262;
  ;; src/options/config/flags.rs:15-22; src/constants.rs:8-10.
  (cl-weave:it "aligns framed CPU header, gap, rows, and click targets with BTM"
    (let* ((sample (getf (%f1-cpu-fixture-series) :sample))
           (config (ntop.shared.application:make-application-config))
           (screen (cl-tty-kit:make-screen 7 7))
           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 7 :height 6))
           (areas
             (let ((ntop.cpu.presentation::*cpu-legend-external-frame-p* t))
               (ntop.cpu.presentation::%draw-cpu-legend
                screen rectangle (ntop.ui:make-table-state) sample config "default"))))
      (cl-weave:expect
       (subseq (cl-tty-kit:screen-row-string screen 1) 1 6)
       :to-equal "CPU  ")
      (cl-weave:expect
       (subseq (cl-tty-kit:screen-row-string screen 2) 1 6)
       :to-equal "     ")
      (cl-weave:expect
       (subseq (cl-tty-kit:screen-row-string screen 3) 1 6)
       :to-equal "All  ")
      (cl-weave:expect
       (ntop.ui:clickable-area-action (first areas))
       :to-be
       :select-row)
      (cl-weave:expect
       (ntop.ui:rectangle-y
        (ntop.ui:clickable-area-rectangle (first areas)))
       :to-be
       3))))

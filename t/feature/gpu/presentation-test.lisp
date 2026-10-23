(in-package #:ntop.test)

(defun %f3-gpu-presentation-series (name value)
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
  (loop for row below (cl-tty-kit:screen-height screen)
        collect (string-right-trim
                 '(#\Space)
                 (cl-tty-kit:screen-row-string screen row))))

(defun %f3-gpu-screen-golden-lines ()
  (with-open-file (stream
                   (asdf:system-relative-pathname
                    "ntop/test"
                    "t/feature/gpu/goldens/presentation-40x16.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(cl-weave:describe "GPU presentation"
  (cl-weave:it "declares the GPU widget through define-widget"
    (cl-weave:expect
     (macroexpand-1
      '(ntop.ui:define-widget :gpu
         (target rectangle state config series)
         :rendered))
     :to-equal
     '(defmethod ntop.ui:draw-widget
          ((ntop.ui::widget-key (eql :gpu))
           target rectangle state config series)
        (declare (ignore ntop.ui::widget-key))
        :rendered)))

  (cl-weave:it "draws No GPU when both series groups are empty"
    (let* ((screen (cl-tty-kit:make-screen 30 8))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 30 :height 8))
           (config (ntop.shared.application:make-application-config)))
      (ntop.ui:draw-widget :gpu screen rectangle nil config
                           '(:usage nil :memory #()))
      (cl-weave:expect
       (not (null (search "No GPU" (cl-tty-kit:screen-to-string screen))))
       :to-be t)))

  (cl-weave:it "renders deterministic fixture histories to the screen golden"
    (let* ((screen (cl-tty-kit:make-screen 40 16))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 16))
           (config
             (ntop.shared.application:make-application-config :theme :default))
           (usage
             (list (%f3-gpu-presentation-series "AMD Radeon" 50d0)))
           (memory
             (list (%f3-gpu-presentation-series "AMD Radeon" 20d0)))
           (rows nil)
           (golden-lines nil))
      (ntop.ui:draw-widget :gpu screen rectangle nil config
                           (list :usage usage :memory memory))
      (setf rows (%f3-gpu-screen-rows screen)
            golden-lines (%f3-gpu-screen-golden-lines))
      (cl-weave:expect (length golden-lines) :to-be 16)
      (cl-weave:expect
       (not (null (some (lambda (line) (plusp (length line))) golden-lines)))
       :to-be t)
      (cl-weave:expect (every (lambda (line) (<= (length line) 40)) golden-lines)
                       :to-be t)
      (cl-weave:expect rows :to-equal golden-lines))))

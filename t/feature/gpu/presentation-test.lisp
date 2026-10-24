(in-package #:ntop.test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames
         "helpers-presentation.lisp"
         (uiop:pathname-directory-pathname
          (asdf:component-pathname
           (asdf:find-component "ntop/test"
                                "t/feature/gpu/presentation-test"))))))

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

  (cl-weave:it "selects a label for every empty GPU availability reason"
    (dolist (entry '((:not-present "No GPU")
                     (:disabled "N/A")
                     (:permission-denied "N/A")
                     (:unsupported-platform "N/A")
                     (:private-api-unavailable "N/A")
                     (:read-failed "N/A")
                     (:missing "N/A")
                     (:unknown "N/A")))
      (let* ((reason (first entry))
             (expected (second entry))
             (screen (cl-tty-kit:make-screen 30 8))
             (rectangle
               (ntop.ui:make-rectangle :x 0 :y 0 :width 30 :height 8))
             (config (ntop.shared.application:make-application-config))
             (series (list :usage nil :memory nil)))
        (unless (eq reason :missing)
          (setf (getf series :unavailability-reason) reason))
        (ntop.ui:draw-widget :gpu screen rectangle nil config series)
        (let ((rendered (cl-tty-kit:screen-to-string screen)))
          (cl-weave:expect
           (list (not (null (search expected rendered)))
                 (not (null
                       (search (if (string= expected "No GPU")
                                   "N/A"
                                   "No GPU")
                               rendered))))
           :to-equal
           (list t nil))))))

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
                           (list :usage usage
                                 :memory memory
                                 :unavailability-reason :not-present))
      (setf rows (%f3-gpu-screen-rows screen)
            golden-lines (%f3-gpu-screen-golden-lines))
      (cl-weave:expect
       (list (length golden-lines)
             (not (null (some (lambda (line) (plusp (length line))) golden-lines)))
             (every (lambda (line) (<= (length line) 40)) golden-lines)
             rows)
       :to-equal
       (list 16 t t golden-lines)))))
(cl-weave:describe "GPU presentation edge layouts"
  (cl-weave:it "skips No GPU drawing for zero-width and zero-height rectangles"
    (let* ((screen (cl-tty-kit:make-screen 30 8))
           (config (ntop.shared.application:make-application-config))
           (fill-count 0)
           (write-count 0))
      (cl-weave:with-mocked-functions
        (((symbol-function 'cl-tty-kit:screen-fill-rect)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (incf fill-count)))
         ((symbol-function 'cl-tty-kit:screen-write-string)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (incf write-count))))
        (ntop.ui:draw-widget
         :gpu screen
         (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 8)
         nil config
         '(:usage nil :memory nil))
        (ntop.ui:draw-widget
         :gpu screen
         (ntop.ui:make-rectangle :x 0 :y 0 :width 30 :height 0)
         nil config
         '(:usage nil :memory nil)))
      (cl-weave:expect fill-count :to-be 0)
      (cl-weave:expect write-count :to-be 0)))

  (cl-weave:it "skips a GPU title when the rectangle has no width"
    (let* ((screen (cl-tty-kit:make-screen 30 4))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 4))
           (write-count 0))
      (cl-weave:with-mocked-functions
        (((symbol-function 'cl-tty-kit:screen-write-string)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (incf write-count))))
        (ntop.gpu.presentation::%draw-gpu-title
         screen rectangle 0 "GPU utilization (%)" "default"))
      (cl-weave:expect write-count :to-be 0)))

  (cl-weave:it "omits the memory chart when only one row is available"
    (let* ((screen (cl-tty-kit:make-screen 40 1))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 1))
           (config (ntop.shared.application:make-application-config))
           (series (list (%f3-gpu-presentation-series "AMD Radeon" 50d0)))
           (rendered nil))
      (ntop.ui:draw-widget :gpu screen rectangle nil config
                           (list :usage series :memory series))
      (setf rendered (cl-tty-kit:screen-to-string screen))
      (cl-weave:expect (not (null (search "GPU utilization (%)" rendered)))
                       :to-be t)
      (cl-weave:expect (not (null (search "GPU memory (%)" rendered)))
                       :to-be nil))))

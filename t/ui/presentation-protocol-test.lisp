(in-package #:ntop.test)

(ntop.ui::define-widget :test-widget (target rectangle state config series)
  (declare (ignore target state config series))
  (list (ntop.ui::make-clickable-area
         :rectangle rectangle
         :action :activate
         :value :process)))

(defmethod ntop.ui::translate-input
    ((raw-event (eql :handled-test-event)) input-context on-meaning
     on-unhandled)
  (declare (ignore input-context on-unhandled))
  (funcall on-meaning :semantic-event))

(defmethod ntop.ui::translate-input
    ((raw-event (eql :unhandled-test-event)) input-context on-meaning
     on-unhandled)
  (declare (ignore input-context on-meaning))
  (funcall on-unhandled raw-event))

(cl-weave:describe "widget and input protocols"
  (cl-weave:it "expands a widget definition into an eql-specialized method"
    (cl-weave:expect
     (macroexpand-1
      '(ntop.ui::define-widget :cpu
         (target rectangle state config series)
         :rendered))
     :to-equal
     '(defmethod ntop.ui::draw-widget
          ((ntop.ui::widget-key (eql :cpu))
           target rectangle state config series)
        (declare (ignore ntop.ui::widget-key))
        :rendered)))

  (cl-weave:it "returns clickable areas from the specialized widget method"
    (let* ((rectangle
             (ntop.ui::make-rectangle :x 2 :y 3 :width 10 :height 4))
           (areas
             (ntop.ui::draw-widget :test-widget :opaque-target rectangle
                                   :state :config :series))
           (area (first areas)))
      (cl-weave:expect
       (list (length areas)
             (ntop.ui::rectangle-x rectangle)
             (ntop.ui::rectangle-y rectangle)
             (ntop.ui::rectangle-width rectangle)
             (ntop.ui::rectangle-height rectangle))
       :to-equal '(1 2 3 10 4))
      (cl-weave:expect (ntop.ui::clickable-area-rectangle area)
                       :to-be rectangle)
      (cl-weave:expect
       (list (ntop.ui::clickable-area-action area)
             (ntop.ui::clickable-area-value area))
       :to-equal '(:activate :process))))

  (cl-weave:it "passes a semantic event to the meaning continuation"
    (let ((meaning nil)
          (unhandled nil))
      (ntop.ui::translate-input
       :handled-test-event :input-context
       (lambda (event) (setf meaning event))
       (lambda (event) (setf unhandled event)))
      (cl-weave:expect meaning :to-be :semantic-event)
      (cl-weave:expect unhandled :to-be nil)))

  (cl-weave:it "passes the original event to the unhandled continuation"
    (let ((meaning nil)
          (unhandled nil))
      (ntop.ui::translate-input
       :unhandled-test-event :input-context
       (lambda (event) (setf meaning event))
       (lambda (event) (setf unhandled event)))
      (cl-weave:expect meaning :to-be nil)
      (cl-weave:expect unhandled :to-be :unhandled-test-event))))

(in-package #:ntop.ui)

(defstruct rectangle
  x
  y
  width
  height)

(defstruct clickable-area
  rectangle
  action
  value)

(defgeneric draw-widget (widget-key target rectangle state config series)
  (:documentation
   "Render synchronously into opaque TARGET and return clickable areas."))

(defmacro define-widget (key (target rectangle state config series) &body body)
  `(defmethod draw-widget
       ((widget-key (eql ,key)) ,target ,rectangle ,state ,config ,series)
     (declare (ignore widget-key))
     ,@body))

(defgeneric translate-input (raw-event input-context on-meaning on-unhandled)
  (:documentation
   "Translate RAW-EVENT and call one continuation with its semantic or raw event."))

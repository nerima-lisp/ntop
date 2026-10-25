(in-package #:ntop.process.presentation)

(defun %draw-process-termination-error-dialog (target rectangle state error-message)
  (let* ((termination-state
           (process-termination-dialog-state-termination-state state))
         (pids
           (ntop.process.application:process-termination-state-target-pids
            termination-state))
         (name (process-termination-dialog-state-process-name state))
         (title " Error ")
         (lines
           (list (if (= (length pids) 1)
                     (format nil "Failed to kill process ~A (~D):"
                             name (first pids))
                     (format nil
                             "Signal delivery stopped while killing processes named ~A:"
                             name))
                 error-message
                 "Please press ENTER or ESC to close this dialog."))
         (content-width
           (loop for line in lines
                 maximize (ntop.ui:measure-table-text line)
                   into longest
                 finally (return (or longest 0))))
         (width
           (min (ntop.ui:rectangle-width rectangle)
                (max 12
                     (+ 2 (ntop.ui:measure-table-text title))
                     (+ 2 content-width))))
         (height (+ 2 (length lines)))
         (screen-height (ntop.ui:rectangle-height rectangle))
         (x (+ (ntop.ui:rectangle-x rectangle)
               (max 0
                    (floor (- (ntop.ui:rectangle-width rectangle) width) 2))))
         (y (+ (ntop.ui:rectangle-y rectangle)
               (max 0 (floor (- screen-height height) 2))))
         (inner-x (1+ x))
         (inner-width (max 0 (- width 2))))
    (when (and (plusp width) (>= screen-height 3))
      (%draw-process-dialog-frame target x y width height title)
      (loop for line in lines
            for row-y from (1+ y)
            do (%draw-dialog-line target inner-x row-y inner-width line :normal)))))

(ntop.ui:define-widget :process-termination-dialog
                       (target rectangle state config sample)
                       (declare (ignore config))
                       (let ((error-message
                               (process-termination-dialog-state-error-message state)))
                         (if error-message
                             (%draw-process-termination-error-dialog
                              target rectangle state error-message)
                             (%draw-process-termination-dialog target
                                                               rectangle
                                                               state
                                                               sample))))

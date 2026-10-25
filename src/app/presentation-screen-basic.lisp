(in-package #:ntop.app.presentation)

(defun %screen-basic-table-selection (state)
  (let ((selection
          (gethash :basic-table-selection
                   (ntop.app.domain:app-state-widget-v-states state))))
    (if (member selection (quote (:disks :process :temperature))
                :test (function eq))
        selection
        :process)))

(defun %screen-basic-table-heading (state)
  (case (%screen-basic-table-selection state)
    (:disks (list "◄ Temperature" "Process ►"))
    (:process (list "◄ Disks" "Temperature ►"))
    (:temperature (list "◄ Process" "Disks ►"))))

(ntop.ui:define-widget :basic-table-arrows
                       (target rectangle state config series)
                       (declare (ignore state))
                       (let ((width (ntop.ui:rectangle-width rectangle))
                             (height (ntop.ui:rectangle-height rectangle)))
                         (when (and (plusp width) (plusp height))
                           (let* ((available-width (max 0 (- width 2)))
                                  (right-text
                                    (ntop.ui:truncate-table-text
                                     (or (second series) "Temperature ►")
                                     available-width))
                                  (right-width
                                    (ntop.ui::chart-text-width right-text))
                                  (left-text
                                    (ntop.ui:truncate-table-text
                                     (or (first series) "◄ Disks")
                                     (max 0 (- available-width right-width))))
                                  (left-x (1+ (ntop.ui:rectangle-x rectangle)))
                                  (right-x
                                    (- (+ (ntop.ui:rectangle-x rectangle) width)
                                       right-width
                                       1))
                                  (y (+ (ntop.ui:rectangle-y rectangle) (1- height)))
                                  (theme
                                    (ntop.shared.application:application-config-theme
                                     config))
                                  (style
                                    (ntop.ui:presentation-theme-style
                                     theme
                                     :highlighted-border)))
                             (unless (zerop (length left-text))
                               (cl-tty-kit:screen-write-string target
                                                               left-x
                                                               y
                                                               left-text
                                                               :style
                                                               style))
                             (unless (zerop (length right-text))
                               (cl-tty-kit:screen-write-string target
                                                               right-x
                                                               y
                                                               right-text
                                                               :style
                                                               style))))
                         nil))

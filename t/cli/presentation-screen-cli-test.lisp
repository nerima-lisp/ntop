(in-package #:ntop.test)

(cl-weave:describe
 "CLI fixture screen behavior"
 (cl-weave:it
  "keeps default and expanded Memory fixture screens unchanged by --free_arc"
  (flet ((screen-cells (screen)
                       (loop for y below (cl-tty-kit:screen-height screen)
                             collect
                             (loop for x below (cl-tty-kit:screen-width screen)
                                   for cell = (cl-tty-kit:screen-cell screen x y)
                                   collect
                                   (list (cl-tty-kit:cell-char cell)
                                         (cl-tty-kit:cell-style cell)))))
         (capture-screen (arguments columns rows selected-widget)
                         (let* ((calls 0)
                                (captured-config nil)
                                (captured-screen nil)
                                (ntop.cli::*application-runner*
                                 (lambda (config collectors clock render
                                                 &key on-action on-snapshot history-stores)
                                   (declare (ignore collectors clock render on-action
                                                    on-snapshot history-stores))
                                   (incf calls)
                                   (setf captured-config config)
                                   (multiple-value-bind
                                       (areas screen)
                                       (%wp12b-screen-render
                                        columns rows
                                        :state
                                        (and selected-widget
                                             (ntop.app.domain:make-app-state
                                              :selected-widget selected-widget))
                                        :config config)
                                     (declare (ignore areas))
                                     (setf captured-screen screen)))))
                           (let ((status
                                  (let ((*standard-output* (make-string-output-stream))
                                        (*error-output* (make-string-output-stream)))
                                    (ntop.cli::%run-cli (cons "ntop" arguments)))))
                             (values status calls captured-config captured-screen)))))
    (dolist (layout '((nil nil) (t :memory)))
      (destructuring-bind (expanded selected-widget) layout
        (dolist (size '((80 24) (160 48)))
          (destructuring-bind (columns rows) size
            (let ((arguments (when expanded '("--expanded"))))
              (multiple-value-bind
                  (base-status base-calls base-config base-screen)
                  (capture-screen arguments columns rows selected-widget)
                (multiple-value-bind
                    (flagged-status flagged-calls flagged-config flagged-screen)
                    (capture-screen
                     (append arguments '("--free_arc"))
                     columns rows selected-widget)
                  (cl-weave:expect base-status :to-equal 0)
                  (cl-weave:expect base-calls :to-equal 1)
                  (cl-weave:expect
                   (ntop.shared.application:application-config-expanded-p base-config)
                   :to-equal expanded)
                  (cl-weave:expect flagged-status :to-equal 0)
                  (cl-weave:expect flagged-calls :to-equal 1)
                  (cl-weave:expect
                   (ntop.shared.application:application-config-expanded-p flagged-config)
                   :to-equal expanded)
                  (cl-weave:expect (screen-cells flagged-screen)
                                   :to-equal
                                   (screen-cells base-screen))))))))))))

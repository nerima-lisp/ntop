(in-package #:ntop.test)

(cl-weave:describe "application screen config modes"
                   (cl-weave:it
                    "uses a basic config with the default application state"
                    (let* ((*wp12b-screen-widget-calls* nil)
                           (*wp12b-screen-recording-p* t)
                           (config (%wp12b-screen-config)))
                      (setf
                       (ntop.shared.application:application-config-basic-p
                        config)
                       t)
                      (%wp12b-screen-render 80 24 :config config)
                      (cl-weave:expect
                       (not
                       (null
                        (member :basic-table-arrows
                                *wp12b-screen-widget-calls*
                                :key (function first)
                                :test (function eq))))
                       :to-be
                       t))))

(in-package #:ntop.test)

(cl-weave:describe "application configuration"
                   (cl-weave:it "expands define-config to a defstruct with keyword defaults"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.shared.application::define-config sample-config
                                                                           make-sample-config
                                                                           ((enabled-p t)
                                                                            (label :ready))))
                                 :to-equal
                                 '(defstruct
                                      (sample-config
                                       (:constructor make-sample-config
                                                     (&key (enabled-p t) (label :ready))))
                                    (enabled-p t)
                                    (label :ready))))
                   (cl-weave:it-each
                    (("rate-ms" 700)
                     ("retention-seconds" 600)
                     ("time-window-seconds" 60)
                     ("zoom-window-seconds" 15)
                     ("hide-time-p" nil)
                     ("auto-hide-time-p" nil)
                     ("theme" :default)
                     ("default-widget-type" :process)
                     ("default-widget-count" 1)
                     ("temperature-unit" :celsius)
                     ("memory-legend-position" :top-right)
                     ("enable-cache-memory-p" nil)
                     ("short-gpu-names-p" nil)
                     ("data-unit" :bit)
                     ("binary-prefix-p" nil)
                     ("network-legend-position" :top-right)
                     ("network-use-log-p" nil)
                     ("network-use-old-legend-p" nil)
                     ("network-show-packets-p" nil)
                     ("network-start-zeroed-p" nil)
                     ("network-interface-filter" nil)
                     ("default-cpu-entry" :all)
                     ("hide-average-cpu-p" nil)
                     ("cpu-left-legend-p" nil)
                     ("unnormalized-cpu-p" nil)
                     ("current-usage-p" nil)
                     ("process-regex-p" nil)
                     ("process-case-sensitive-p" nil)
                     ("process-whole-word-p" nil)
                     ("process-get-threads-p" nil)
                     ("process-hide-kernel-threads-p" nil)
                     ("process-tree-collapse-p" nil)
                     ("process-show-command-p" nil)
                     ("process-default-memory-value-p" nil)
                     ("process-default-grouped-p" nil)
                     ("process-default-tree-p" nil)
                     ("process-columns" nil)
                     ("process-sort-column" :cpu-percent)
                     ("process-sort-order" :descending)
                     ("disable-advanced-kill-p" nil)
                     ("battery-p" nil)
                     ("disable-click-p" nil)
                     ("disable-keys-p" nil)
                     ("read-only-p" nil)
                     ("basic-p" nil)
                     ("expanded-p" nil)
                     ("show-table-scroll-position-p" nil)
                     ("disable-gpu-p" nil)
                     ("dot-marker-p" nil)
                     ("widget-rows"
                      (((:cpu 2) (:memory 1))
                       ((:network 2) (:disk 2) (:temperature 1))
                       ((:process 2)))))
                    "defaults ~A to ~S"
                    (slot-name expected)
                    (let* ((config (ntop.shared.application::make-application-config))
                           (accessor-name
                            (concatenate 'string "APPLICATION-CONFIG-" (string-upcase slot-name)))
                           (accessor
                            (intern accessor-name (find-package "NTOP.SHARED.APPLICATION"))))
                      (cl-weave:expect (funcall accessor config) :to-equal expected)))
                   (cl-weave:it "allows keyword overrides and preserves untouched defaults"
                                (let ((config
                                       (ntop.shared.application::make-application-config :rate-ms
                                                                                         250
                                                                                         :basic-p
                                                                                         t)))
                                  (cl-weave:expect
                                   (ntop.shared.application::application-config-rate-ms config)
                                   :to-equal
                                   250)
                                  (cl-weave:expect
                                   (ntop.shared.application::application-config-basic-p config)
                                   :to-be-truthy)
                                  (cl-weave:expect
                                   (ntop.shared.application::application-config-retention-seconds
                                    config)
                                   :to-equal
                                   600)))
                   (cl-weave:it "overrides process columns"
                                (let ((config
                                       (ntop.shared.application::make-application-config
                                        :process-columns
                                        (quote (:pid :name)))))
                                  (cl-weave:expect
                                   (ntop.shared.application::application-config-process-columns
                                    config)
                                   :to-equal
                                   (quote (:pid :name))))))

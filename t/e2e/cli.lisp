(in-package #:ntop.e2e)

(defparameter *measurement-target-choices*
  (quote ("btm")))

(defparameter *measurement-name-choices*
  '("n1" "n2" "n3" "n4" "n5" "n6"))

(defparameter *comparison-scope-choices*
  (quote ("condition-a")))

(defparameter *route-mode-choices* '("ntop" "btm-self"))

(defun command-goldens (invocation)
  (capture-btm-goldens :output
                       (cl-cli:option-value invocation :output)
                       :btm
                       (cl-cli:option-value invocation :btm)
                       :rate
                       (cl-cli:option-value invocation :rate)
                       :timeout-seconds
                       (cl-cli:option-value invocation :timeout-seconds)))

(defun %handle-compare (invocation)
  (compare-goldens :left
                   (cl-cli:positional-value invocation :left)
                   :right
                   (cl-cli:positional-value invocation :right)
                   :scope
                   (cl-cli:option-value invocation :scope)))

(defun %handle-measure (invocation)
  (run-measurement :target
                   (cl-cli:option-value invocation :target)
                   :measurement
                   (cl-cli:option-value invocation :measurement)
                   :rows
                   (cl-cli:option-value invocation :rows)
                   :columns
                   (cl-cli:option-value invocation :columns)
                   :rate
                   (cl-cli:option-value invocation :rate)
                   :runs
                   (cl-cli:option-value invocation :runs)
                   :duration-seconds
                   (cl-cli:option-value invocation :duration-seconds)
                   :timeout-seconds
                   (cl-cli:option-value invocation :timeout-seconds)))

(defun %handle-export-parity (invocation)
  (export-domain-parity :output (cl-cli:option-value invocation :output)))

(defun %handle-n7 (invocation)
  (run-n7 :root (cl-cli:option-value invocation :root)))

(defun %handle-routes (invocation)
  (run-route-harness :mode (cl-cli:option-value invocation :mode)
                     :btm (cl-cli:option-value invocation :btm)
                     :ntop (cl-cli:option-value invocation :ntop)
                     :evidence-directory
                     (cl-cli:option-value invocation :evidence-directory)
                     :rows (cl-cli:option-value invocation :rows)
                     :columns (cl-cli:option-value invocation :columns)
                     :rate (cl-cli:option-value invocation :rate)
                     :timeout-seconds
                     (cl-cli:option-value invocation :timeout-seconds)
                     :route-ids (cl-cli:option-value invocation :route)
                     :subcase-id (cl-cli:option-value invocation :subcase)))

(cl-cli:define-app *e2e-app*
                   (:name "ntop-e2e"
                          :summary
                          "Capture btm goldens, run route checks, and measure workloads."
                          :require-command
                          t)
                   (:command "goldens"
                             (:description
                              "Capture the btm golden grid at every specified terminal size."
                              :handler
                              #'command-goldens)
                             (:option "output"
                                      :kind
                                      :value
                                      :default
                                      "t/e2e/goldens"
                                      :value-hint
                                      :dir
                                      :description
                                      "Golden output directory.")
                             (:option "btm"
                                      :kind
                                      :value
                                      :default
                                      "btm"
                                      :value-hint
                                      :file
                                      :description
                                      "btm executable.")
                             (:option "rate"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      700
                                      :description
                                      "Sampling interval in milliseconds.")
                             (:option "timeout-seconds"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      30
                                      :description
                                      "Maximum wait for each process and frame."))
                   (:command "compare"
                             (:description
                              (concatenate 'string
                                           "Compare a baseline golden set with a candidate "
                                           "capture using FR-001 condition A.")
                              :handler
                              #'%handle-compare)
                             (:option "scope"
                                      :kind
                                      :value
                                      :choices
                                      *comparison-scope-choices*
                                      :default
                                      "condition-a"
                                      :description
                                      "FR-001 acceptance condition A.")
                             (:positional :left
                                          :required-p
                                          t
                                          :value-hint
                                          :dir
                                          :description
                                          "Baseline golden set.")
                             (:positional :right
                                          :required-p
                                          t
                                          :value-hint
                                          :dir
                                          :description
                                          "Candidate capture."))
                   (:command "measure"
                             (:description
                              "Run one bounded N1-N6 measurement against btm."
                              :handler
                              #'%handle-measure)
                             (:option "measurement"
                                      :kind
                                      :value
                                      :choices
                                      *measurement-name-choices*
                                      :required-p
                                      t
                                      :description
                                      (concatenate 'string
                                                   "N1 CPU, N2 RSS, N3 btm version, "
                                                   "N4 key response, N5 resize response, "
                                                   "N6 terminal mode."))
                             (:option "target"
                                      :kind
                                      :value
                                      :choices
                                      *measurement-target-choices*
                                      :default
                                      "btm"
                                      :description
                                      "The WP0-6 runner supports btm only.")
                             (:option "rows"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      48
                                      :description
                                      "Terminal height in rows.")
                             (:option "columns"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      160
                                      :description
                                      "Terminal width in columns.")
                             (:option "rate"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      700
                                      :description
                                      "Sampling interval in milliseconds.")
                             (:option "runs"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      1
                                      :description
                                      (concatenate 'string
                                                   "Number of runs. N1 and N2 sample "
                                                   "for the specified duration."))
                             (:option "duration-seconds"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      60
                                      :description
                                      "Sampling window per run for N1 CPU and N2 RSS (seconds).")
                             (:option "timeout-seconds"
                                      :kind
                                      :value
                                      :type
                                      :integer
                                      :min
                                      1
                                      :default
                                      90
                                      :description
                                      "Maximum wait for each process and measurement."))
                   (:command "export-parity"
                             (:description
                              "Export the typed WP0-5 btm compatibility catalog as JSON."
                              :handler
                              #'%handle-export-parity)
                             (:option "output"
                                      :kind
                                      :value
                                      :default
                                      "-"
                                      :value-hint
                                      :file
                                      :description
                                      "JSON destination, or - for standard output."))
                   (:command "n7"
                             (:description
                              "Check golden files for machine-specific data."
                              :handler
                              #'%handle-n7)
                             (:option "root"
                                      :kind
                                      :value
                                      :default
                                      "t/e2e/goldens"
                                      :value-hint
                                      :dir
                                      :description
                                      (concatenate 'string
                                                   "On macOS, scan the golden directory "
                                                   "for host-specific data.")))
                   (:command "routes"
  (:description
   "Run selected routes from the typed catalog against ntop and btm."
   :handler #'%handle-routes)
  (:option "mode"
           :kind :value
           :choices *route-mode-choices*
           :default "ntop"
           :description "Run ntop against btm, or run btm against itself.")
  (:option "route"
           :kind :value
           :multiple-p t
           :description "Route ID to run. Repeat to select multiple routes.")
  (:option "subcase"
           :kind :value
           :description "Single subcase ID, for example key-15-002.")
  (:option "btm"
           :kind :value
           :default "btm"
           :value-hint :file
           :description "Reference btm executable.")
  (:option "ntop"
           :kind :value
           :default "./ntop"
           :value-hint :file
           :description "Candidate ntop executable.")
  (:option "evidence-directory"
           :kind :value
           :default "/tmp"
           :value-hint :dir
           :description "Parent directory for route evidence.")
  (:option "rows"
           :kind :value
           :type :integer
           :min 1
           :default 60
           :description "Route terminal height in rows.")
  (:option "columns"
           :kind :value
           :type :integer
           :min 1
           :default 220
           :description "Route terminal width in columns.")
  (:option "rate"
           :kind :value
           :type :integer
           :min 1
           :default 250
           :description "Sampling interval in milliseconds.")
  (:option "timeout-seconds"
           :kind :value
           :type :integer
           :min 1
           :default 60
           :description "Maximum wait for each route child.")))

(defun main (&optional (argv (cl-cli:application-argv)))
  (sb-ext:exit :code (cl-cli:run-app *e2e-app* :argv argv)))

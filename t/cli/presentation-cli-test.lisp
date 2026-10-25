(in-package #:ntop.test)

(defun cli-test-flags ()
  (remove-if-not
   (lambda (entry)
     (eq (ntop.app.domain:parity-entry-kind entry) :flag))
   (ntop.app.domain:btm-parity-catalog)))

(defun cli-test-spellings (entry)
  (let ((short (ntop.app.domain:parity-flag-short entry)))
    (remove-duplicates
     (append
      (loop for name in (append
                         (list (ntop.app.domain:parity-flag-snake-case entry)
                               (ntop.app.domain:parity-flag-kebab-case entry))
                         (ntop.app.domain:parity-flag-aliases entry))
            collect (concatenate 'string "--" (string-left-trim "-" name)))
      (when (and short (plusp (length short)))
        (list short)))
     :test
     #'string-equal)))

(defun cli-test-sample-value (entry)
  (let ((name (ntop.app.domain:parity-flag-snake-case entry))
        (type (ntop.app.domain:parity-flag-value-type entry)))
    (cond
      ((member type '("boolean" "control") :test #'string=) nil)
      ((string= type "duration")
       (cond
         ((string= name "rate") "251ms")
         ((string= name "default_time_value") "30s")
         ((string= name "retention") "60s")
         ((string= name "time_delta") "1s")))
      ((string= type "integer") "2")
      ((string= type "enum") (first (ntop.app.domain:parity-flag-values entry)))
      ((string= type "path") "/tmp/ntop-config")
      ((string= type "column-name")
       (first
        (loop for column in (ntop.app.domain:btm-parity-catalog)
              when (eq (ntop.app.domain:parity-entry-kind column) :column)
                append (ntop.app.domain:parity-column-aliases column)))))))

(defun cli-test-sample-values (entry)
  (let ((type (ntop.app.domain:parity-flag-value-type entry)))
    (cond
      ((string= type "enum") (copy-list (ntop.app.domain:parity-flag-values entry)))
      ((string= type "column-name")
       (remove-duplicates
        (loop for column in (ntop.app.domain:btm-parity-catalog)
              when (eq (ntop.app.domain:parity-entry-kind column) :column)
                append (ntop.app.domain:parity-column-aliases column))
        :test
        #'string-equal))
      ((member type '("boolean" "control") :test #'string=) nil)
      (t (list (cli-test-sample-value entry))))))

(defun cli-test-parse (arguments)
  (cl-cli:parse-argv (ntop.cli::%make-cli-app) (cons "ntop" arguments)))

(defun cli-test-entry-argv (entry spelling &optional value)
  (append (list spelling)
          (unless
              (member (ntop.app.domain:parity-flag-value-type entry)
                      '("boolean" "control")
                      :test
                      #'string=)
            (list value))))

(defun cli-test-invalid-p (arguments)
  (handler-case
      (multiple-value-bind (config status)
          (ntop.cli::%config-from-invocation (cli-test-parse arguments))
        (declare (ignore config))
        (/= status 0))
    (cl-cli:cli-usage-error ()
      t)))

(defun cli-test-captured-run (arguments)
  (let* ((calls 0)
         (captured nil)
         (status nil)
         (stdout nil)
         (stderr nil)
         (ntop.cli::*application-runner*
          (lambda (config collectors clock render
                   &key on-action on-snapshot history-stores)
            (declare (ignore collectors clock render on-action
                             on-snapshot history-stores))
            (incf calls)
            (setf captured config))))
    (setf stdout
          (with-output-to-string (out)
            (setf stderr
                  (with-output-to-string (err)
                    (let ((*standard-output* out)
                          (*error-output* err))
                      (setf status
                            (ntop.cli::%run-cli (cons "ntop" arguments))))))))
    (values status calls captured stdout stderr)))

(defun cli-test-accessor (initarg)
  (intern
   (concatenate 'string "APPLICATION-CONFIG-" (string-upcase (symbol-name initarg)))
   (find-package "NTOP.SHARED.APPLICATION")))

(cl-weave:describe "CLI conditional config"
                   (cl-weave:it
                    "requires a default widget type when a widget count is supplied"
                    (let* ((app (ntop.cli::%make-cli-app))
                           (invocation
                            (cl-cli:parse-argv app '("ntop" "--default_widget_count" "2"))))
                      (multiple-value-bind (config status)
                          (ntop.cli::%config-from-invocation invocation)
                        (declare (ignore config))
                        (cl-weave:expect status :to-equal 2))))
                   (cl-weave:it "accepts each declared short alias"
                                (dolist (entry (cli-test-flags))
                                  (let* ((short (ntop.app.domain:parity-flag-short entry))
                                         (type
                                          (ntop.app.domain:parity-flag-value-type entry)))
                                    (when
                                        (and short
                                             (plusp (length short))
                                             (not (string= type "control")))
                                      (let* ((arguments
                                              (append
                                               (list
                                                (concatenate 'string
                                                             "-"
                                                             (string-left-trim "-" short)))
                                               (unless (string= type "boolean")
                                                 (list (cli-test-sample-value entry)))))
                                             (invocation (cli-test-parse arguments))
                                             (actual
                                              (ntop.cli::%option-value invocation entry)))
                                        (cl-weave:expect
                                         (cl-cli:invocation-action invocation)
                                         :to-equal
                                         :dispatch)
                                        (cl-weave:expect
                                         (if (string= type "boolean")
                                             (eq actual t)
                                             (not
                                              (eq actual ntop.cli::*missing-option-value*)))
                                         :to-be
                                         t)))))))

(cl-weave:describe "CLI parity catalog"
                   (cl-weave:it
                    "accepts every declared value and spelling for all 52 parity flags"
                    (let ((records (cli-test-flags)))
                      (cl-weave:expect (length records) :to-equal 52)
                      (dolist (entry records)
                        (let* ((name (ntop.app.domain:parity-flag-snake-case entry))
                               (type (ntop.app.domain:parity-flag-value-type entry))
                               (samples (cli-test-sample-values entry)))
                          (dolist (spelling (cli-test-spellings entry))
                            (if (string= type "control")
                                (let ((invocation (cli-test-parse (list spelling))))
                                  (cl-weave:expect (cl-cli:invocation-action invocation)
                                                   :to-equal
                                                   (if (string= name "help")
                                                       :help
                                                       :version)))
                                (dolist
                                    (value
                                     (if (string= type "boolean")
                                         (list nil)
                                         samples))
                                  (let* ((arguments
                                          (cli-test-entry-argv entry spelling value))
                                         (invocation (cli-test-parse arguments))
                                         (actual
                                          (ntop.cli::%option-value invocation entry)))
                                    (cl-weave:expect (cl-cli:invocation-action invocation)
                                                     :to-equal
                                                     :dispatch)
                                    (cl-weave:expect
                                     (if (string= type "boolean")
                                         (eq actual t)
                                         (not
                                          (eq actual ntop.cli::*missing-option-value*)))
                                     :to-be
                                     t)))))))))
                   (cl-weave:it
                    "rejects a generated invalid case for each of the 52 parity flags"
                    (dolist (entry (cli-test-flags))
                      (let* ((name (ntop.app.domain:parity-flag-snake-case entry))
                             (type (ntop.app.domain:parity-flag-value-type entry))
                             (spellings (cli-test-spellings entry)))
                        (cond
                          ((string= type "control")
                           (cl-weave:expect
                            (cli-test-invalid-p
                             (list (concatenate 'string "--" name "=invalid")))
                            :to-be
                            t))
                          ((string= type "boolean")
                           (cl-weave:expect
                            (cli-test-invalid-p
                             (list (concatenate 'string "--" name "=false")))
                            :to-be
                            t))
                          ((string= type "path")
                           (dolist (spelling spellings)
                             (cl-weave:expect (cli-test-invalid-p (list spelling))
                                              :to-be
                                              t)))
                          ((string= type "duration")
                           (dolist (spelling spellings)
                             (dolist
                                 (value
                                  (append
                                   (list "abc")
                                   (unless (string= name "retention")
                                     (list
                                      (cond
                                        ((string= name "rate") "249ms")
                                        ((string= name "default_time_value") "29999ms")
                                        ((string= name "time_delta") "999ms"))))))
                               (cl-weave:expect
                                (multiple-value-bind (config status)
                                    (ntop.cli::%config-from-invocation
                                     (cli-test-parse (list spelling value)))
                                  (declare (ignore config))
                                  status)
                                :to-equal
                                1))))
                          (t
                           (let ((value
                                  (cond
                                    ((string= type "integer") "not-an-integer")
                                    ((string= type "enum") "not-a-choice")
                                    ((string= type "column-name") "not-a-column"))))
                             (dolist (spelling spellings)
                               (cl-weave:expect (cli-test-invalid-p (list spelling value))
                                                :to-be
                                                t))))))))
                   (cl-weave:it
                    "uses built-in controls and their short spellings without running"
                    (dolist
                        (case '(("-h" :help) ("--help" :help)
                                ("-V" :version)
                                ("--version" :version)))
                      (let* ((invocation (cli-test-parse (list (first case))))
                             (result
                              (multiple-value-list
                               (cli-test-captured-run (list (first case))))))
                        (cl-weave:expect (cl-cli:invocation-action invocation)
                                         :to-equal
                                         (second case))
                        (cl-weave:expect (first result) :to-equal 0)
                        (cl-weave:expect (second result) :to-equal 0))))
                   (cl-weave:it
                    "accepts catalog aliases and derives both config spellings"
                    (dolist (name '("config_location" "config-location" "config"))
                      (let ((invocation
                             (cli-test-parse
                              (list (concatenate 'string "--" name) "/tmp/config"))))
                        (cl-weave:expect (cl-cli:option-value invocation :config_location)
                                         :to-equal
                                         "/tmp/config"))))
                   (cl-weave:it "maps supplied booleans and preserves config defaults"
                                (let* ((defaults
                                        (ntop.shared.application:make-application-config))
                                       (invocation (cli-test-parse nil))
                                       (config
                                        (ntop.cli::%config-from-invocation invocation)))
                                  (cl-weave:expect
                                   (ntop.shared.application:application-config-rate-ms
                                    config)
                                   :to-equal
                                   (ntop.shared.application:application-config-rate-ms
                                    defaults))
                                  (cl-weave:expect
                                   (ntop.shared.application:application-config-default-widget-type
                                    config)
                                   :to-equal
                                   :process)
                                  (dolist (binding ntop.cli::*boolean-config-bindings*)
                                    (unless
                                        (and (string= (car binding) "hide_k_threads")
                                             (not (member :linux *features*)))
                                      (let* ((entry
                                              (ntop.cli::%flag-by-name (car binding)))
                                             (invocation
                                              (cli-test-parse
                                               (list
                                                (concatenate 'string
                                                             "--"
                                                             (ntop.app.domain:parity-flag-snake-case
                                                              entry)))))
                                             (config
                                              (ntop.cli::%config-from-invocation
                                               invocation))
                                             (accessor (cli-test-accessor (cdr binding))))
                                        (cl-weave:expect (funcall accessor config)
                                                         :to-be
                                                         t))))))
                   (cl-weave:it
                    "maps durations, units, widget aliases, legends, and enum values"
                    (multiple-value-bind (config status)
                        (ntop.cli::%config-from-invocation
                         (cli-test-parse
                          '("--rate" "500"
                                     "--retention"
                                     "120s"
                                     "--default_time_value"
                                     "45s"
                                     "--time_delta"
                                     "2s"
                                     "--network_use_bytes"
                                     "--network_use_binary_prefix"
                                     "--fahrenheit"
                                     "--theme"
                                     "nord"
                                     "--default_widget_type"
                                     "net"
                                     "--default_widget_count"
                                     "2"
                                     "--memory_legend"
                                     "left"
                                     "--network_legend"
                                     "bottom"
                                     "--default_cpu_entry"
                                     "avg")))
                      (cl-weave:expect status :to-equal 0)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-rate-ms config)
                       :to-equal
                       500)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-retention-seconds
                        config)
                       :to-equal
                       120)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-time-window-seconds
                        config)
                       :to-equal
                       45)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-zoom-window-seconds
                        config)
                       :to-equal
                       2)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-data-unit config)
                       :to-equal
                       :byte)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-binary-prefix-p config)
                       :to-be
                       t)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-temperature-unit
                        config)
                       :to-equal
                       :fahrenheit)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-theme config)
                       :to-equal
                       :nord)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-default-widget-type
                        config)
                       :to-equal
                       :network)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-default-widget-count
                        config)
                       :to-equal
                       2)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-memory-legend-position
                        config)
                       :to-equal
                       :left)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-network-legend-position
                        config)
                       :to-equal
                       :bottom)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-default-cpu-entry
                        config)
                       :to-equal
                       :average)))
                   (cl-weave:it "returns required status codes for invalid input"
  (cl-weave:expect
   (first (multiple-value-list (cli-test-captured-run '("--unknown"))))
   :to-equal 2)
  (cl-weave:expect
   (first (multiple-value-list (cli-test-captured-run '("--theme" "invalid"))))
   :to-equal 2)
  (dolist (entry (cli-test-flags))
    (when (string= (ntop.app.domain:parity-flag-value-type entry) "duration")
      (let ((name (ntop.app.domain:parity-flag-snake-case entry)))
        (cl-weave:expect
         (multiple-value-bind (config status)
             (ntop.cli::%config-from-invocation
              (cli-test-parse (list (concatenate 'string "--" name) "abc")))
           (declare (ignore config))
           status)
         :to-equal 1))))
  (dolist (arguments '(("--process_default_sort" "not-a-column")
                      ("--rate" "100")))
    (cl-weave:expect
     (first (multiple-value-list (cli-test-captured-run arguments)))
     :to-equal 1))
  (dolist (arguments '(("--rate" "250")
                      ("--rate" "250ms")
                      ("--retention" "59999ms")))
    (cl-weave:expect
     (multiple-value-bind (config status)
         (ntop.cli::%config-from-invocation (cli-test-parse arguments))
       (declare (ignore config))
       status)
     :to-equal 0))
  (multiple-value-bind (status calls config stdout stderr)
      (cli-test-captured-run '("--rate" "abc"))
    (declare (ignore calls config))
    (cl-weave:expect status :to-equal 1)
    (cl-weave:expect stdout :to-equal "")
    (cl-weave:expect (plusp (length stderr)) :to-be t))
  (cl-weave:expect
   (first (multiple-value-list (cli-test-captured-run '("--hide_k_threads"))))
   :to-equal (if (member :linux *features*) 0 2))
  (cl-weave:expect
   (multiple-value-bind (config status)
       (ntop.cli::%config-from-invocation
        (cli-test-parse '("--celsius" "--kelvin")))
     (declare (ignore config))
     status)
   :to-equal 2))
                   (cl-weave:it
                    "accepts free_arc as a no-op and calls the application runner"
                    (multiple-value-bind (status calls config)
                        (cli-test-captured-run '("--free_arc"))
                      (cl-weave:expect status :to-equal 0)
                      (cl-weave:expect calls :to-equal 1)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-rate-ms config)
                       :to-equal
                       700)
                      (cl-weave:expect
                       (ntop.shared.application:application-config-default-widget-type
                        config)
                       :to-equal
                       :process))))

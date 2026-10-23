(in-package #:ntop.cli)

(defparameter *missing-option-value*
  (gensym "MISSING"))

(defparameter *boolean-config-bindings*
  '(("autohide_time" . :auto-hide-time-p) ("basic" . :basic-p)
    ("disable_click" . :disable-click-p)
    ("disable_keys" . :disable-keys-p)
    ("dot_marker" . :dot-marker-p)
    ("expanded" . :expanded-p)
    ("hide_time" . :hide-time-p)
    ("show_table_scroll_position" . :show-table-scroll-position-p)
    ("case_sensitive" . :process-case-sensitive-p)
    ("current_usage" . :current-usage-p)
    ("disable_advanced_kill" . :disable-advanced-kill-p)
    ("read_only" . :read-only-p)
    ("hide_k_threads" . :process-hide-kernel-threads-p)
    ("get_threads" . :process-get-threads-p)
    ("group_processes" . :process-default-grouped-p)
    ("process_memory_as_value" . :process-default-memory-value-p)
    ("process_command" . :process-show-command-p)
    ("regex" . :process-regex-p)
    ("tree" . :process-default-tree-p)
    ("tree_collapse" . :process-tree-collapse-p)
    ("unnormalized_cpu" . :unnormalized-cpu-p)
    ("whole_word" . :process-whole-word-p)
    ("cpu_left_legend" . :cpu-left-legend-p)
    ("hide_avg_cpu" . :hide-average-cpu-p)
    ("enable_cache_memory" . :enable-cache-memory-p)
    ("short_gpu_names" . :short-gpu-names-p)
    ("network_use_binary_prefix" . :binary-prefix-p)
    ("network_use_log" . :network-use-log-p)
    ("use_old_network_legend" . :network-use-old-legend-p)
    ("show_packets" . :network-show-packets-p)
    ("network_start_zeroed" . :network-start-zeroed-p)
    ("battery" . :battery-p)
    ("disable_gpu" . :disable-gpu-p)))

(defun %option-value (invocation entry)
  (cl-cli:option-value invocation
                       (intern
                        (string-upcase (ntop.app.domain:parity-flag-snake-case entry))
                        :keyword)
                       *missing-option-value*))

(defun %provided-p (invocation entry)
  (not (eq (%option-value invocation entry) *missing-option-value*)))

(defun %flag-by-name (name)
  (find name
        (%flag-records)
        :key
        #'ntop.app.domain:parity-flag-snake-case
        :test
        #'string=))

(defun %provided-name-p (invocation name)
  (let ((entry (%flag-by-name name)))
    (and entry (%provided-p invocation entry))))

(defun %column-sort-key (value invocation)
  (let* ((columns
          (remove-if-not
           (lambda (entry)
             (and (eq (ntop.app.domain:parity-entry-kind entry) :column)
                  (member value
                          (ntop.app.domain:parity-column-aliases entry)
                          :test
                          #'string-equal)))
           (ntop.app.domain:btm-parity-catalog)))
         (memory-as-value (%provided-name-p invocation "process_memory_as_value"))
         (column
          (if (and (member value '("mem" "mem%" "memory" "memory%") :test #'string-equal)
                   (> (length columns) 1))
              (find
               (if memory-as-value
                   "MemValue"
                   "MemPercent")
               columns
               :key
               #'ntop.app.domain:parity-column-field
               :test
               #'string=)
              (first columns))))
    (when column
      (let ((field (ntop.app.domain:parity-column-field column)))
        (cond
          ((string= field "CpuPercent") :cpu-percent)
          ((member field '("MemValue" "MemPercent") :test #'string=) :memory)
          ((string= field "VirtualMem") :virtual-memory-bytes)
          ((string= field "Pid") :pid)
          ((string= field "Count") :count)
          ((string= field "Name") :name)
          ((string= field "Command") :command)
          ((string= field "ReadPerSecond") :read-bytes-per-second)
          ((string= field "WritePerSecond") :write-bytes-per-second)
          ((string= field "TotalRead") :total-read-bytes)
          ((string= field "TotalWrite") :total-write-bytes)
          ((string= field "State") :state)
          ((string= field "User") :user)
          ((string= field "Time") :runtime-seconds)
          ((string= field "Nice") :nice)
          ((string= field "Priority") :priority)
          ((member field '("GpuMemValue" "GpuMemPercent") :test #'string=) :gpu-memory)
          ((string= field "GpuUtilPercent") :gpu-usage-percent))))))

(defparameter *duration-units*
  '(("milliseconds" . 1) ("millisecond" . 1)
    ("msecs" . 1)
    ("msec" . 1)
    ("ms" . 1)
    ("microseconds" . 1/1000)
    ("microsecond" . 1/1000)
    ("usec" . 1/1000)
    ("us" . 1/1000)
    ("nanoseconds" . 1/1000000)
    ("nanosecond" . 1/1000000)
    ("nsec" . 1/1000000)
    ("ns" . 1/1000000)
    ("minutes" . 60000)
    ("minute" . 60000)
    ("mins" . 60000)
    ("min" . 60000)
    ("m" . 60000)
    ("seconds" . 1000)
    ("second" . 1000)
    ("secs" . 1000)
    ("sec" . 1000)
    ("s" . 1000)
    ("hours" . 3600000)
    ("hour" . 3600000)
    ("hrs" . 3600000)
    ("hr" . 3600000)
    ("h" . 3600000)
    ("days" . 86400000)
    ("day" . 86400000)
    ("d" . 86400000)
    ("weeks" . 604800000)
    ("week" . 604800000)
    ("w" . 604800000)
    ("years" . 31536000000)
    ("year" . 31536000000)
    ("yr" . 31536000000)
    ("y" . 31536000000)))

(defun %skip-spaces (text position)
  (loop while (and (< position (length text))
                   (find (char text position) '(#\Space #\Tab)))
        do (incf position)
        finally (return position)))

(defun %ascii-digit-p (character)
  (and (char<= #\0 character) (char<= character #\9)))

(defun %duration-unit-at (text position)
  (find-if
   (lambda (unit)
     (let ((name (car unit)))
       (and (<= (+ position (length name)) (length text))
            (string-equal name text :start2 position :end2 (+ position (length name))))))
   *duration-units*))

(defun %parse-duration-ms (text)
  (when (stringp text)
    (let ((length (length text))
          (position 0)
          (total 0)
          (components 0))
      (setf position (%skip-spaces text position))
      (when (= position length)
        (return-from %parse-duration-ms
          nil))
      (loop while (< position length)
            do (let ((start position))
                 (loop while (and (< position length)
                                  (%ascii-digit-p (char text position)))
                       do (incf position))
                 (when (= start position)
                   (return-from %parse-duration-ms
                     nil))
                 (let ((number (parse-integer text :start start :end position)))
                   (setf position (%skip-spaces text position))
                   (if (= position length)
                       (if (zerop components)
                           (return-from %parse-duration-ms
                             number)
                           (return-from %parse-duration-ms
                             nil))
                       (let ((unit (%duration-unit-at text position)))
                         (unless unit
                           (return-from %parse-duration-ms
                             nil))
                         (incf total (* number (cdr unit)))
                         (incf components)
                         (incf position (length (car unit)))
                         (setf position (%skip-spaces text position))))))
            finally (return total)))))

(defun %duration-minimum-ms (name)
  (cond
    ((string= name "default_time_value") 30000)
    ((string= name "rate") 250)
    ((string= name "retention") 60000)
    ((string= name "time_delta") 1000)))

(defun %duration-config-binding (name value)
  (let* ((milliseconds (%parse-duration-ms value))
         (minimum (%duration-minimum-ms name)))
    (when
        (or (null milliseconds)
            (if (string= name "rate")
                (<= milliseconds minimum)
                (< milliseconds minimum)))
      (return-from %duration-config-binding
        (values nil
                nil
                (if (string= name "rate")
                    1
                    2))))
    (values
     (cond
       ((string= name "rate") :rate-ms)
       ((string= name "retention") :retention-seconds)
       ((string= name "default_time_value") :time-window-seconds)
       (t :zoom-window-seconds))
     (if (string= name "rate")
         milliseconds
         (/ milliseconds 1000))
     0)))

(defun %widget-type-value (value)
  (cond
    ((string-equal value "cpu") :cpu)
    ((member value '("mem" "memory") :test #'string-equal) :memory)
    ((member value '("net" "network") :test #'string-equal) :network)
    ((member value '("proc" "process" "processes") :test #'string-equal) :process)
    ((member value '("temp" "temperature") :test #'string-equal) :temperature)
    ((string-equal value "disk") :disk)
    ((member value '("batt" "battery") :test #'string-equal) :battery)))

(defun %temperature-value (invocation)
  (let ((selected
         (loop for name in '("celsius" "fahrenheit" "kelvin")
               when (%provided-name-p invocation name)
                 collect name)))
    (when (> (length selected) 1)
      (return-from %temperature-value
        (values nil 2)))
    (values
     (cond
       ((equal (first selected) "fahrenheit") :fahrenheit)
       ((equal (first selected) "kelvin") :kelvin)
       (selected :celsius))
     0)))

(defun %config-binding (name value invocation)
  (let ((boolean-binding (assoc name *boolean-config-bindings* :test #'string=)))
    (when boolean-binding
      (return-from %config-binding
        (values (cdr boolean-binding) value 0))))
  (cond
    ((member name '("default_time_value" "rate" "retention" "time_delta") :test #'string=)
     (%duration-config-binding name value))
    ((string= name "default_widget_count") (values :default-widget-count value 0))
    ((string= name "default_widget_type")
     (values :default-widget-type (%widget-type-value value) 0))
    ((string= name "process_default_sort")
     (let ((sort-key (%column-sort-key value invocation)))
       (values :process-sort-column (or sort-key :cpu-percent) 0)))
    ((string= name "default_cpu_entry")
     (values :default-cpu-entry
             (if (member value '("avg" "average") :test #'string-equal)
                 :average
                 :all)
             0))
    ((member name '("memory_legend" "network_legend") :test #'string=)
     (values
      (if (string= name "memory_legend")
          :memory-legend-position
          :network-legend-position)
      (intern (string-upcase value) :keyword)
      0))
    ((string= name "network_use_bytes") (values :data-unit :byte 0))
    ((member name '("celsius" "fahrenheit" "kelvin") :test #'string=)
     (multiple-value-bind (unit status) (%temperature-value invocation)
       (values :temperature-unit unit status)))
    ((string= name "theme") (values :theme (intern (string-upcase value) :keyword) 0))
    ((string= name "config_location") (values nil nil 2))
    (t (values nil nil 0))))

(defun %config-from-invocation (invocation)
  (when (%provided-name-p invocation "config_location")
    (return-from %config-from-invocation
      (values nil 2)))
  (when
      (and (%provided-name-p invocation "hide_k_threads")
           (not (member :linux *features*)))
    (return-from %config-from-invocation
      (values nil 2)))
  (when
      (and (%provided-name-p invocation "default_widget_count")
           (not (%provided-name-p invocation "default_widget_type")))
    (return-from %config-from-invocation
      (values nil 2)))
  (multiple-value-bind (temperature status) (%temperature-value invocation)
    (declare (ignore temperature))
    (when (/= status 0)
      (return-from %config-from-invocation
        (values nil status))))
  (let ((initargs nil))
    (dolist (entry (%flag-records))
      (when (%provided-p invocation entry)
        (multiple-value-bind (key value code)
            (%config-binding (ntop.app.domain:parity-flag-snake-case entry)
                             (%option-value invocation entry)
                             invocation)
          (unless (zerop code)
            (return-from %config-from-invocation
              (values nil code)))
          (when key
            (setf initargs (append initargs (list key value)))))))
    (values (apply #'ntop.shared.application:make-application-config initargs) 0)))

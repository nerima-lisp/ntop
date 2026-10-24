(in-package #:ntop.e2e)

(defparameter *route-frame-quiet-seconds* 0.3d0)
(defparameter *route-first-frame-factor* 3d0)

(defun %route-temporary-directory (parent)
  (loop for directory =
          (merge-pathnames
           (format nil "ntop-routes-~d-~d/" (get-universal-time)
                   (random 1000000000))
           (uiop:ensure-directory-pathname (pathname parent)))
        unless (probe-file directory)
          do (ensure-directories-exist (merge-pathnames ".claim" directory))
             (let ((claim (open (merge-pathnames ".claim" directory)
                                :direction :output
                                :if-exists nil
                                :if-does-not-exist :create)))
               (when claim
                 (close claim)
                 (return directory)))))

(defun %route-environment ()
  (cons "TERM=xterm-256color"
        (remove-if (lambda (entry)
                     (and (>= (length entry) 5)
                          (string= entry "TERM=" :end1 5)))
                   (sb-ext:posix-environ))))

(defun %route-octets (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun %route-key-input (key)
  (cond
    ((string= key "Ctrl+C") (string (code-char 3)))
    ((string= key "Ctrl+R") (string (code-char 18)))
    ((string= key "Ctrl+F") (string (code-char 6)))
    ((string= key "Ctrl+Left") (format nil "~c[1;5D" #\Escape))
    ((string= key "Ctrl+Right") (format nil "~c[1;5C" #\Escape))
    ((string= key "Ctrl+Up") (format nil "~c[1;5A" #\Escape))
    ((string= key "Ctrl+Down") (format nil "~c[1;5B" #\Escape))
    ((string= key "Shift+Left") (format nil "~c[1;2D" #\Escape))
    ((string= key "Shift+Right") (format nil "~c[1;2C" #\Escape))
    ((string= key "Shift+Up") (format nil "~c[1;2A" #\Escape))
    ((string= key "Shift+Down") (format nil "~c[1;2B" #\Escape))
    ((string= key "Alt+h") (format nil "~ch" #\Escape))
    ((string= key "Alt+l") (format nil "~cl" #\Escape))
    ((string= key "Alt+c") (format nil "~cc" #\Escape))
    ((string= key "Alt+C") (format nil "~cC" #\Escape))
    ((string= key "Alt+w") (format nil "~cw" #\Escape))
    ((string= key "Alt+W") (format nil "~cW" #\Escape))
    ((string= key "Alt+r") (format nil "~cr" #\Escape))
    ((string= key "Alt+R") (format nil "~cR" #\Escape))
    ((string= key "Home") (format nil "~c[H" #\Escape))
    ((string= key "End") (format nil "~c[F" #\Escape))
    ((string= key "PageUp") (format nil "~c[5~~" #\Escape))
    ((string= key "PageDown") (format nil "~c[6~~" #\Escape))
    ((string= key "Delete") (format nil "~c[3~~" #\Escape))
    ((string= key "Backspace") (string (code-char 127)))
    ((string= key "Up") (format nil "~c[A" #\Escape))
    ((string= key "Down") (format nil "~c[B" #\Escape))
    ((string= key "Left") (format nil "~c[D" #\Escape))
    ((string= key "Right") (format nil "~c[C" #\Escape))
    ((string= key "F1") (format nil "~cOP" #\Escape))
    ((string= key "F2") (format nil "~cOQ" #\Escape))
    ((string= key "F3") (format nil "~cOR" #\Escape))
    ((string= key "F5") (format nil "~c[15~~" #\Escape))
    ((string= key "F6") (format nil "~c[17~~" #\Escape))
    ((string= key "F9") (format nil "~c[20~~" #\Escape))
    ((string= key "Enter") (string #\Return))
    ((string= key "Escape") (string #\Escape))
    ((string= key "Tab") (string #\Tab))
    ((string= key "Space") (string #\Space))
    ((string= key "Ctrl+A") (string (code-char 1)))
    ((string= key "Ctrl+E") (string (code-char 5)))
    ((string= key "Ctrl+U") (string (code-char 21)))
    ((string= key "Ctrl+D") (string (code-char 4)))
    ((string= key "Ctrl+H") (string (code-char 8)))
    ((string= key "Ctrl+W") (string (code-char 23)))
    ((string= key "gg") "gg")
    ((string= key "dd") "dd")
    ((string= key "printable characters") "sleep")
    ((string= key "Shift+Q") "Q")
    (t key)))

(defun %route-mouse-input (gesture rows columns)
  (let* ((x (max 1 (min columns (floor columns 2))))
         (y (max 1 (min rows (floor rows 2))))
         (event (cond
                  ((string= gesture "wheel-up") (list 64 #\M))
                  ((string= gesture "wheel-down") (list 65 #\M))
                  ((string= gesture "left-button-down") (list 0 #\M))
                  ((string= gesture "right/middle/other button")
                   (list 2 #\M))
                  ((string= gesture "move/drag/release") (list 32 #\m))
                  (t (error "Unknown route mouse gesture: ~s" gesture)))))
    (format nil "~c[<~d;~d;~d~c" #\Escape (first event) x y (second event))))

(defun %route-widget-type (context)
  (cond
    ((member context '("Memory" "memory widget") :test #'string-equal) "mem")
    ((member context '("Temperatures" "temperature table")
             :test #'string-equal) "temp")
    ((member context '("Disks" "disk table") :test #'string-equal) "disk")
    ((member context '("Network" "network table") :test #'string-equal) "net")
    ((member context '("Processes" "process table" "tables" "process tree"
                       "process sort" "process sort list" "process search"
                       "search cursor" "kill dialog")
             :test #'string-equal) "proc")
    ((member context '("battery widget" "battery tabs") :test #'string-equal)
     "battery")
    ((string-equal context "CPU legend") "cpu")
    ((string-equal context "graphs") "cpu")))

(defun %route-context-input (context)
  (cond
    ((string-equal context "help") "?")
    ((string-equal context "help search") "?/")
    ((string-equal context "process search") "/")
    ((string-equal context "search cursor") "/sleep")
    ((string-equal context "process sort") "s")
    ((string-equal context "process sort list") "s")
    ((string-equal context "process tree") "t")
    ((string-equal context "expanded widget") "e")
    (t "")))

(defun %route-safe-column-inputs (entry)
  (let ((condition (ntop.app.domain:parity-column-condition entry)))
    (cond
      ((search "when process_memory_as_value" condition)
       (list "--process-memory-as-value"))
      ((search "when process_command" condition) (list "--process-command"))
      ((search "when grouped" condition) (list "--group-processes"))
      (t nil))))

(defun %route-safe-context-args (context)
  (unless (string-equal context "normal process table")
    (let ((type (%route-widget-type context)))
      (when type
        (list "--default-widget-type" type "--default-widget-count" "1")))))

(defun %route-flag-switch (entry variant &optional alias)
  (let ((name (ecase variant
                (:kebab (ntop.app.domain:parity-flag-kebab-case entry))
                (:snake (ntop.app.domain:parity-flag-snake-case entry))
                (:short (ntop.app.domain:parity-flag-short entry))
                (:alias alias))))
    (unless (and (stringp name) (plusp (length name)))
      (error "Flag ~a has no spelling for ~s."
             (ntop.app.domain:parity-flag-snake-case entry) variant))
    (cond ((eq variant :short)
           (concatenate 'string "-" (string-left-trim "-" name)))
          ((eq variant :alias) name)
          (t (concatenate 'string "--" (string-left-trim "-" name))))))

(defun %route-flag-value (entry evidence-directory)
  (let ((name (ntop.app.domain:parity-flag-snake-case entry))
        (type (ntop.app.domain:parity-flag-value-type entry))
        (values (ntop.app.domain:parity-flag-values entry)))
    (cond
      ((string= type "boolean") nil)
      ((string= name "default_widget_count") "1")
      ((string= name "default_widget_type") "cpu")
      ((string= name "config_location")
       (namestring (merge-pathnames "btm-route-config.toml"
                                    evidence-directory)))
      ((string= name "rate") "1s")
      ((string= name "default_time_value") "30s")
      ((string= name "retention") "60s")
      ((string= name "time_delta") "1s")
      ((string= type "integer") "1")
      ((string= type "column-name") "pid")
      ((member type '("enum" "string" "path") :test #'string=)
       (or (first values) "ntop-route"))
      ((string= type "control") nil)
      (t (error "No safe value for parity flag ~a of type ~a." name type)))))

(defun %route-flag-arguments (route subcase evidence-directory)
  (let* ((entry (route-entry route))
         (variant (getf subcase :variant))
         (switch (%route-flag-switch entry variant (getf subcase :alias)))
         (value (%route-flag-value entry evidence-directory))
         (name (ntop.app.domain:parity-flag-snake-case entry))
         (type (ntop.app.domain:parity-flag-value-type entry))
         (prefix (when (string= name "default_widget_count")
                   (list "--default-widget-type" "cpu"))))
    (append prefix
            (if (getf subcase :invalid-value)
                (cond
                  ((member type '("boolean" "control") :test #'string=)
                   (list (concatenate 'string switch "=__ntop_invalid__")))
                  ((member type '("path" "string") :test #'string=)
                   (list switch))
                  (t (list switch "__ntop_invalid__")))
                (append (list switch) (when value (list value)))))))

(defun %route-subcases-expanded (route)
  (loop for subcase in (route-subcases route)
        append (if (getf subcase :inputs)
                   (loop for input in (getf subcase :inputs)
                         collect (append (copy-list subcase)
                                         (list :input input)))
                   (list subcase))))

(defun %route-extension-action-sequence (route subcase)
  (let ((key (getf subcase :input))
        (context (getf subcase :context)))
    (cond
      ((and (string= (route-action route) "Cycle selected widget view")
            (string= key "v"))
       (make-list (if (string= context "Temperatures") 3 2)
                  :initial-element "v"))
      ((string= key "]") (list "[" "]"))
      ((string= key "G") (list "[" "G"))
      (t (list key)))))

(defun %route-input-description (route subcase)
  (let* ((context (or (getf subcase :context) "CLI"))
         (input (or (getf subcase :input)
                    (getf subcase :gesture)
                    (getf subcase :variant)))
         (sequence (when (eq (route-kind route) :extension)
                     (%route-extension-action-sequence route subcase)))
         (flag-name (when (eq (route-kind route) :flag)
                      (ntop.app.domain:parity-flag-snake-case
                       (route-entry route))))
         (flag-arguments
           (when flag-name
             (%route-flag-arguments route subcase #p"/tmp/")))
         (flag
           (when flag-name
             (format nil "~{~a~^ ~}"
                     (if (and (string= flag-name "config_location")
                              (not (getf subcase :invalid-value)))
                         (append (butlast flag-arguments)
                                 (list "<evidence>/btm-route-config.toml"))
                         flag-arguments))))
         (action
           (cond ((and flag-name (string= flag-name "disable_keys"))
                  "press ?; help remains closed")
                 ((and flag-name (string= flag-name "disable_click"))
                  "click Memory; CPU remains selected")
                 ((and flag-name (string= flag-name "read_only"))
                  "Delete; owned child stays alive and no dialog opens")
                 ((and flag-name
                       (string= flag-name "disable_advanced_kill"))
                  "Delete; owned child gets a simple Yes/No dialog")
                 ((eq (route-kind route) :key) (route-action route)))))
    (format nil "~a~@[ / ~a~]~@[ / ~a~]"
            context
            (cond (sequence (format nil "~{~a~^ then ~}" sequence))
                  (flag flag)
                  ((eq (route-kind route) :search-field)
                   (%route-search-query (getf subcase :input)))
                  (t input))
            action)))

(defun %route-action-text (route subcase rows columns)
  (case (route-kind route)
    (:key (%route-key-input (getf subcase :input)))
    (:mouse
     (if (string-equal (or (getf subcase :context) "")
                       "basic-mode arrows")
         :basic-mode-right-arrow
         (%route-mouse-input (getf subcase :gesture) rows columns)))
    (:search-field (%route-search-query (getf subcase :input)))
    (:extension (%route-extension-action-sequence route subcase))
    (:flag
     (let ((name (ntop.app.domain:parity-flag-snake-case
                  (route-entry route))))
       (cond ((string= name "disable_keys") (%route-key-input "?"))
             ((string= name "disable_click") :focus-memory)
             ((member name '("read_only" "disable_advanced_kill")
                      :test #'string=)
              (%route-key-input "Delete"))
             (t ""))))
    (:additional
     (unless (member (route-id route) '("basic-mode" "resize" "sleep-child")
                     :test #'string=)
       (getf subcase :input)))
    (otherwise "")))

(defun %route-base-arguments (program config-path route subcase
                              evidence-directory)
  (let* ((context (or (getf subcase :context) ""))
         (flag (when (eq (route-kind route) :flag)
                 (ntop.app.domain:parity-flag-snake-case
                  (route-entry route))))
         (process-control
           (member flag '("disable_keys" "read_only" "disable_advanced_kill")
                   :test #'string=)))
    (append
     (when (string= (pathname-name (pathname program)) "btm")
       (list "-C" (namestring config-path)))
     (case (route-kind route)
       (:flag (%route-flag-arguments route subcase evidence-directory))
       (:column (%route-safe-column-inputs (route-entry route)))
       (:additional
        (cond ((string= (route-id route) "basic-mode") (list "--basic"))
              ((string= (route-id route) "sleep-child")
               (list "--default-widget-type" "proc"
                     "--default-widget-count" "1"))))
       (otherwise nil))
     (when (and (eq (route-kind route) :mouse)
                (string-equal context "basic-mode arrows"))
       (list "--basic"))
     (when (or (string-equal context "kill dialog")
               (string= (route-id route) "sleep-child")
               process-control)
       (list "--process-command"))
     (when process-control
       (list "--default-widget-type" "proc" "--default-widget-count" "1"))
     (%route-safe-context-args context))))

(defun %route-write-ansi (path octets)
  (%write-octets path octets)
  (namestring path))

(defun %route-grid (octets rows columns)
  (let ((grid (make-terminal-grid :rows rows :cols columns)))
    (parse-ansi (%octets-to-text octets)
                grid
                (lambda (value) value)
                (lambda (condition) (error condition)))))

(defun %route-grid-text (grid)
  (with-output-to-string (stream)
    (dotimes (row (grid-height grid))
      (dotimes (column (grid-width grid))
        (write-char (cell-character (grid-cell-at grid row column)) stream))
      (terpri stream))))

(defun %route-projection-first-difference (left right)
  (when (and left right (not (criterion-projections-equal-p left right)))
    (let ((left-cells (aref left 2))
          (right-cells (aref right 2)))
      (loop for row below (min (aref left 1) (aref right 1))
            do (loop for column below (min (aref left 0) (aref right 0))
                     for left-cell =
                       (find-if (lambda (cell)
                                  (and (= row (aref cell 1))
                                       (= column (aref cell 2))))
                                left-cells)
                     for right-cell =
                       (find-if (lambda (cell)
                                  (and (= row (aref cell 1))
                                       (= column (aref cell 2))))
                                right-cells)
                     unless (equalp left-cell right-cell)
                       do (return-from %route-projection-first-difference
                            (format nil "row ~d column ~d: reference ~s; candidate ~s"
                                    (1+ row) (1+ column)
                                    left-cell right-cell))))
      (format nil "projection dimensions or cell count differ: ~d x ~d vs ~d x ~d"
              (aref left 0) (aref left 1) (aref right 0) (aref right 1)))))

(defun run-route-harness (&key (mode "ntop") (btm "btm") (ntop "./ntop")
                               (evidence-directory "/tmp")
                               (rows 60) (columns 220) (rate 250)
                               (timeout-seconds 30))
  (unless (member mode '("ntop" "btm-self") :test #'string=)
    (error "Unknown route mode: ~s" mode))
  (let* ((directory (%route-temporary-directory evidence-directory))
         (config-path (merge-pathnames "btm-route-config.toml" directory))
         (report-path (merge-pathnames "report.md" directory))
         (routes (route-catalog))
         (results nil)
         (reference-program btm)
         (candidate-program (if (string= mode "btm-self") btm ntop)))
    (unless (= (length routes) 182)
      (error "Expected 182 routes, found ~d." (length routes)))
    (%write-text config-path (btm-config-string rate))
    (loop for route in routes
          for route-number from 1
          do (let* ((subcases (%route-subcases-expanded route))
                    (subresults nil)
                    (route-inputs nil))
               (cond
                ((and (string= mode "btm-self")
                      (not (route-comparable-p route)))
                 (loop for subcase in subcases
                       for subcase-index from 1
                       do (push (%route-input-description route subcase)
                                route-inputs)
                       (push (%route-noncomparable-manual-result
                              route subcase subcase-index directory)
                             subresults)))
                ((route-manual-reason route)
                 (let ((subcase (first subcases)))
                   (push (%route-input-description route subcase) route-inputs)
                   (setf subresults
                         (list (%route-manual-result route subcase directory)))))
                (t
                 (loop for subcase in subcases
                       for subcase-index from 1
                       do (push (%route-input-description route subcase)
                                route-inputs)
                       (handler-case
                           (push (%route-side-pair
                                  route subcase subcase-index mode
                                  reference-program candidate-program
                                  directory config-path rows columns rate
                                  timeout-seconds)
                                 subresults)
                         (error (condition)
                                (let ((error-path
                                       (merge-pathnames
                                        (format nil "~a-~3,'0d-error.txt"
                                                (route-id route) subcase-index)
                                        directory)))
                                  (%write-text error-path
                                               (princ-to-string condition))
                                  (push (list :passed nil
                                              :input (%route-input-description
                                                      route subcase)
                                              :expected (route-expected route)
                                              :actual (princ-to-string condition)
                                              :cause "route execution raised an error"
                                              :first-difference nil
                                              :evidence
                                              (list (namestring error-path)))
                                        subresults)))))))
               (setf subresults (nreverse subresults))
               (let* ((first-difference
                       (find-if (lambda (subresult)
                                  (getf subresult :first-difference))
                                subresults))
                      (result
                       (list :id (route-id route)
                             :status (%route-result-status route subresults)
                             :input (format nil "~{~a~^; ~}"
                                            (nreverse route-inputs))
                             :expected (route-expected route)
                             :actual (format nil "~{~a~^; ~}"
                                             (mapcar
                                              (lambda (subresult)
                                                (getf subresult :actual))
                                              subresults))
                             :first-difference
                             (and first-difference
                                  (getf first-difference :first-difference))
                             :cause (format nil "~{~a~^; ~}"
                                            (mapcar
                                             (lambda (subresult)
                                               (getf subresult :cause))
                                             subresults))
                             :evidence
                             (remove nil
                                     (mapcan
                                      (lambda (subresult)
                                        (copy-list (getf subresult :evidence)))
                                      subresults))
                             :subresults subresults)))
                 (push result results)
                 (format t "~d/~d ~a ~a~%"
                         route-number (length routes) (route-id route)
                         (string-upcase
                          (symbol-name (%route-result-status route subresults)))))))
    (setf results (nreverse results))
    (%route-write-report report-path mode reference-program candidate-program
                         results directory)
    (format t "Route report: ~a~%" (namestring report-path))
    (format t "Evidence directory: ~a~%" (namestring directory))
    (if (every (lambda (result)
                 (member (getf result :status) '(:pass :manual)))
               results)
        0
      1)))

(defun %route-search-query (token)
  (cond ((null token) "sleep")
        ((member token '("state" "user") :test #'string=)
         (format nil "~a!=__ntop_no_match__" token))
        ((string= token "time") "time>=0s")
        ((member token
                 '("pid" "cpu" "cpu%" "memb" "mem" "mem%" "read" "r/s"
                   "rps" "write" "w/s" "wps" "tread" "t.read" "twrite"
                   "t.write" "nice" "priority" "gmem" "gmem%" "gpu%")
                 :test #'string=)
         (format nil "~a>=0" token))
        (t token)))

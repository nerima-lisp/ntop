(in-package #:ntop.e2e)

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

(defun %route-context-input (context &optional route subcase)
  (cond
    ((and route (string-equal context "normal")
          (member (route-id route) '("key-10" "key-11") :test #'string=))
     (if (string= (route-id route) "key-10") "J" "K"))
    ((and route
          (string-equal context "help")
          (member (route-id route)
                  '("key-12" "key-13" "key-14" "key-15"
                    "key-16" "key-17" "key-18" "key-19")
                  :test #'string=))
     (cond
       ((member (route-id route) '("key-12" "key-13" "key-14")
                :test #'string=)
        "?2")
       ((member (route-id route) '("key-15" "key-17" "key-19")
                :test #'string=)
        "?1")
       (t "?9")))
    ((and route
          (string-equal context "help search")
          (member (route-id route) '("key-26" "key-27" "key-35" "key-36")
                  :test #'string=))
     "?/process")
    ((and route
          (string= (route-id route) "key-27")
          (string-equal context "process search")
          (string= (getf subcase :input) "F3"))
     "/btm")
    ((string-equal context "help") "?")
    ((string-equal context "help search") "?/")
    ((string-equal context "process search") "/")
    ((string-equal context "search cursor") "/sleep")
    ((string-equal context "process sort") "s")
    ((string-equal context "process sort list") "s")
    ((string-equal context "process tree") "t")
    ((string-equal context "expanded widget") "e")
    (t "")))

(defun %route-help-search-query-visible-p (grid query)
  (and grid
       (let ((prompt (format nil "Search: ~a" query))
             (prompt-visible nil)
             (match-visible nil))
         (loop for row below (grid-height grid)
               for line = (criterion-row-text grid row)
               do (cond
                    ((search prompt line :test (function char-equal))
                     (setf prompt-visible t))
                    ((and (search query line :test (function char-equal))
                          (search " - " line)
                          (not (search "Search:" line
                                       :test (function char-equal))))
                     (setf match-visible t))))
         (and prompt-visible match-visible))))
(defun %route-search-toggle-state (route subcase initial grid stable)
  (cond
    ((and (member (route-id route) (quote ("key-26" "key-27"))
                  :test (function string=))
          (string-equal (or (getf subcase :context) "") "help search")
          (member (getf subcase :input) (quote ("Alt+W" "Alt+R"))
                  :test (function string=)))
     (and stable
          (%route-help-search-query-visible-p initial "process")
          (%route-help-search-query-visible-p grid "process")))
    ((and (string= (route-id route) "key-27")
          (string-equal (or (getf subcase :context) "") "process search")
          (string= (or (getf subcase :input) "") "F3")
          (%route-widget-no-data-p initial "Processes")
          (%route-widget-no-data-p grid "Processes"))
     :manual)
    (t nil)))

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
        (append (list "--default-widget-type" type "--default-widget-count" "1")
                (when (string-equal context "process tree")
                  (list "--tree")))))))

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
  (declare (ignore rows columns)) (case (route-kind route)
    (:key (%route-key-input (getf subcase :input)))
    (:mouse
     (if (string-equal (or (getf subcase :context) "")
                       "basic-mode arrows")
         :basic-mode-right-arrow
         :route-mouse-action))
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

(defun %route-base-arguments (program config-path route subcase evidence-directory)
  (let* ((context (or (getf subcase :context) ""))
         (btm-p (string= (pathname-name (pathname program)) "btm"))
         (flag (when (eq (route-kind route) :flag)
                 (ntop.app.domain:parity-flag-snake-case (route-entry route))))
         (process-control
           (member flag '("disable_keys" "read_only" "disable_advanced_kill")
                   :test #'string=)))
    (append
     (when (and btm-p (not (string= flag "config_location")))
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
     (when (and (eq (route-kind route) :flag)
                (string= flag "expanded"))
       (list "--basic"))
     (when (or (string-equal context "kill dialog")
               (string= (route-id route) "sleep-child")
               process-control)
       (list "--process-command"))
     (when process-control
       (list "--default-widget-type" "proc" "--default-widget-count" "1"))
     (%route-safe-context-args context)
     (when (and (not btm-p)
                (eq (route-kind route) :key)
                (member (route-id route) '("key-9" "key-10" "key-11")
                        :test #'string=))
       (list "--default-widget-type"
             (if (member (route-id route) '("key-9" "key-11")
                         :test #'string=)
                 "cpu"
               "proc")
             "--default-widget-count" "1"))
     (when (and (not btm-p)
                (member context '("battery widget" "battery tabs" "CPU legend")
                        :test #'string-equal))
       (if (string-equal context "CPU legend") '("--expanded")
           '("--battery" "--expanded"))))))



(defun %route-write-ansi (path octets)
  (%write-octets path octets)
  (namestring path))

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
         (format nil "~a~a" token (if (string= token "pid") " 0" ">=0")))
        (t token)))

(defun %route-read-through-quiet-period
    (session &key quiet-seconds timeout-seconds
             (initial-quiet-seconds quiet-seconds)
             completion-predicate
             (completion-samples-required 2)
             side stage stability-seconds (allow-unchanged-p t))
  (%validate-seconds quiet-seconds "QUIET-SECONDS")
  (%validate-seconds initial-quiet-seconds "INITIAL-QUIET-SECONDS")
  (let* ((prefix (copy-seq (e2e-session-output session)))
         (prefix-length (length prefix))
         (rows (e2e-session-rows session))
         (columns (e2e-session-columns session))
         (completion-samples 0)
         (last-completion-gap-count
           (length (e2e-session-output-gaps-seconds session)))
         (*route-frame-capture-not-before-at*
           (unless completion-predicate
             (+ (%now) (%seconds-to-ticks initial-quiet-seconds)))))
    (unless (and rows columns)
      (error "PTY session has no recorded frame dimensions."))
    (multiple-value-bind (cumulative grid metadata)
        (%route-capture-frame
         session prefix rows columns
         :side (or side *route-frame-capture-side* :unknown)
         :stage (or stage :route-read-through-quiet-period)
         :timeout-seconds timeout-seconds
         :stability-seconds
         (if completion-predicate
             0d0
           (or stability-seconds *route-frame-stability-seconds*
               quiet-seconds))
         :allow-unchanged-p
         (and allow-unchanged-p (null completion-predicate))
         :completion-predicate
         (when completion-predicate
           (lambda (grid)
             (declare (ignore grid))
             (let* ((output *route-frame-capture-current-octets*)
                    (new-output (subseq output prefix-length))
                    (gap-count
                      (length
                       (e2e-session-output-gaps-seconds session))))
               (when (> (- gap-count last-completion-gap-count) 1)
                 (setf completion-samples 0))
               (setf last-completion-gap-count gap-count)
               (if (funcall completion-predicate new-output)
                   (incf completion-samples)
                 (setf completion-samples 0))
               (>= completion-samples completion-samples-required)))))
      (declare (ignore grid metadata))
      (values (subseq cumulative prefix-length)
              (%elapsed-seconds session (%now))))))

(in-package #:ntop.e2e)

(defun %route-fixture-decimal (text)
  (let* ((point (position #\. text))
         (whole (parse-integer text :end point))
         (fraction (and point (parse-integer text :start (1+ point)))))
    (float
     (if point
         (+ whole (/ fraction (expt 10 (- (length text) point 1))))
         whole)
     1d0)))

(defun %route-fixture-frequency-values (text)
  (let ((totals (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal)))
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((frequency (search "frequency" line :test #'char-equal))
                      (mhz (search "MHz" line :test #'char-equal))
                      (ghz (search "GHz" line :test #'char-equal))
                      (unit (or mhz ghz)))
                 (when (and frequency unit)
                   (let* ((start (position-if #'digit-char-p line
                                              :start (+ frequency (length "frequency"))))
                          (end
                           (and start
                                (or
                                 (position-if-not
                                  (lambda (char)
                                    (or (digit-char-p char) (char= char #\.)))
                                  line
                                  :start
                                  start)
                                 unit))))
                     (when (and start end (> end start))
                       (let* ((value
                               (* (%route-fixture-decimal (subseq line start end))
                                  (if ghz
                                      1000d0
                                    1d0)))
                              (prefix (string-trim '(#\Space #\Tab #\:) (subseq line 0 frequency)))
                              (cluster
                               (cond
                                ((search "E-Cluster" prefix :test #'char-equal) "E-Cluster")
                                ((search "P-Cluster" prefix :test #'char-equal) "P-Cluster")
                                (t
                                 (if (plusp (length prefix))
                                     prefix
                                   "CPU")))))
                         (incf (gethash cluster totals 0d0) value)
                         (incf (gethash cluster counts 0))))))))
      (sort
       (loop for cluster being the hash-keys of totals
             collect (cons cluster (/ (gethash cluster totals) (gethash cluster counts))))
       #'string<
       :key
       #'car))))

(defun %route-fixture-capability-unavailable-p (text)
  (some
   (lambda (phrase)
     (search phrase text :test #'char-equal))
   '("a password is required" "not allowed to execute"
                              "must be run as root"
                              "permission denied"
                              "not permitted"
                              "no such file"
                              "command not found"
                              "unknown sampler"
                              "sampler not available"
                              "unsupported sampler")))

(defun %route-fixture-powermetrics-sample ()
  (let ((system (software-type)))
    (if (not (search "Darwin" system :test #'char-equal))
        (list :capability
              :unavailable
              :reason
              "powermetrics is Darwin-only"
              :system
              system
              :exit-code
              nil
              :stdout
              ""
              :stderr
              ""
              :values
              nil)
      (handler-case (let* ((result
                            (host-kit:run-program "sudo"
                                                  '("-n" "powermetrics" "--samplers" "cpu_power"
                                                         "-i" "1000" "-n" "5")
                                                  :timeout
                                                  15
                                                  :max-output-characters
                                                  (* 1024 1024)))
                           (stdout (host-kit:process-result-stdout result))
                           (stderr (host-kit:process-result-stderr result))
                           (exit-code (host-kit:process-result-exit-code result))
                           (timed-out (host-kit:process-result-timed-out-p result))
                           (status
                            (cond
                             ((or timed-out
                                  (host-kit:process-result-stdout-truncated-p result)
                                  (host-kit:process-result-stderr-truncated-p result)) :unknown)
                             ((eql exit-code 0) :available)
                             ((%route-fixture-capability-unavailable-p
                               (concatenate 'string stdout "\n" stderr))
                              :unavailable)
                             (t :unknown))))
                      (list :capability
                            status
                            :system
                            system
                            :exit-code
                            exit-code
                            :stdout
                            stdout
                            :stderr
                            stderr
                            :timed-out-p
                            timed-out
                            :values
                            (%route-fixture-frequency-values stdout)))
        (error (condition)
               (let* ((message (princ-to-string condition))
                      (status
                       (if (%route-fixture-capability-unavailable-p message)
                           :unavailable
                         :unknown)))
                 (list :capability
                       status
                       :system
                       system
                       :exit-code
                       nil
                       :stdout
                       ""
                       :stderr
                       message
                       :error
                       message
                       :values
                       nil)))))))

(defun %route-fixture-powermetrics-text (label sample)
  (format nil
          (concatenate 'string
                       "~a~%system: ~a~%capability: ~a~%exit-code: ~a~%"
                       "timed-out: ~a~%reason: ~a~%values-MHz: ~s~%"
                       "stdout:~%~a~%stderr:~%~a~%")
          label
          (getf sample :system)
          (getf sample :capability)
          (getf sample :exit-code)
          (getf sample :timed-out-p)
          (getf sample :reason)
          (getf sample :values)
          (getf sample :stdout)
          (getf sample :stderr)))

(defun %route-fixture-battery-tabs (grid frame)
  (when (and grid frame)
    (destructuring-bind (top bottom left right) frame
      (loop with labels = nil
            for row from top below bottom
            for text = (subseq (criterion-row-text grid row)
                               left
                               (min (grid-width grid) (1+ right)))
            do (loop with start = 0
                     for position = (search "Battery " text :start2 start :test #'char-equal)
                     while position
                     for number-start = (+ position (length "Battery "))
                     for number-end =
                       (or (position-if-not #'digit-char-p text :start number-start)
                           (length text))
                     do (when (> number-end number-start)
                          (pushnew (subseq text number-start number-end)
                                   labels :test #'string=))
                        (setf start (max number-end (1+ position))))
            finally (return (nreverse labels))))))

(progn
  (defun %route-fixture-prepare-mouse-capability
      (state route subcase raw rows columns)
    (let* ((grid (%route-grid raw rows columns))
           (copy (copy-list subcase))
           (inputs nil)
           (other nil))
      (setf (getf copy :gesture) "right/middle/other button"
            inputs (%route-mouse-action-inputs route copy grid rows columns)
            other
            (handler-case
                (%route-mouse-subcase-inputs
                 route (list* :button :other copy) grid rows columns)
              (error (condition) (list :error (princ-to-string condition)))))
      (let* ((right (and inputs (list (first inputs))))
             (middle (and (cdr inputs) (list (second inputs))))
             (error-text (and (eq (first other) :error) (second other)))
             (capability
               (cond
                 ((or (null right) (null middle)) :unknown)
                 (error-text
                  (if (search "cannot encode Other button-down" error-text
                              :test #'char-equal)
                      :unavailable
                      :unknown))
                 ((null other) :unknown)
                 ((or (equal other right) (equal other middle)) :unavailable)
                 (t :available)))
             (diagnostic
               (or error-text
                   (case capability
                     (:available "Other input is distinct from Right and Middle.")
                     (:unavailable "Other input duplicates another button.")
                     (otherwise "Could not verify the mouse input encoding.")))))
        (setf (getf state :mouse-other-capability) capability
              (getf state :mouse-other-inputs)
              (list :right right :middle middle :other other)
              (getf state :mouse-other-diagnostic) diagnostic
              (getf state :skip-actions) (eq capability :unavailable)))))

  (defun %route-fixture-query-extra-process-names (subcase)
    (case (getf subcase :query-fixture)
      (:sleep-variants (list "ntop-t-Sleep"))
      ((:help-search :process-search) nil)
      (otherwise
       (when (getf subcase :query-fixture)
         (error "Unknown route query fixture: ~s."
                (getf subcase :query-fixture))))))

  (defun %route-fixture-query-setup-inputs (subcase)
    (let ((procedure (getf subcase :query-procedure))
          (query (getf subcase :query-initial))
          (cursor (getf subcase :query-cursor)))
      (case procedure
        ((nil :field-token) nil)
        ((:toggle-case :toggle-whole-word :toggle-regex
          :cursor-start :cursor-end :clear
          :delete-previous-word :delete-previous-character
          :delete-next-character)
         (unless (stringp query)
           (error "Query procedure ~s requires an initial string." procedure))
         (let ((target (or cursor (length query))))
           (unless (and (integerp target) (<= 0 target (length query)))
             (error "Query cursor ~s is outside the initial string." target))
           (append
            (list (%route-key-input "Ctrl+U") query)
            (loop repeat (- (length query) target)
                  collect (%route-key-input "Left")))))
        (otherwise
         (error "Unknown route query procedure: ~s." procedure)))))

  (defun %route-fixture-query-after-action-input (subcase)
    (let ((sentinel (getf subcase :query-sentinel)))
      (when sentinel
        (unless (stringp sentinel)
          (error "Query sentinel must be a string: ~s." sentinel))
        sentinel))))

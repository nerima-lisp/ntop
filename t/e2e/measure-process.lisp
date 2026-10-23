(in-package #:ntop.e2e)
(defun %read-command-output (program arguments timeout-seconds)
  (let* ((effective-timeout
          (%effective-timeout-seconds timeout-seconds "command"))
         (result (host-kit:run-program
                  program arguments :timeout effective-timeout
                  :max-output-characters 65536))
         (output (host-kit:process-result-stdout result))
         (error-output (host-kit:process-result-stderr result)))
    (when (or (host-kit:process-result-stdout-truncated-p result)
              (host-kit:process-result-stderr-truncated-p result))
      (%measurement-fail "Command output exceeded the capture limit: ~a"
                         program))
    (unless (zerop (host-kit:process-result-exit-code result))
      (%measurement-fail "Command failed (~d): ~a~%~a"
                         (host-kit:process-result-exit-code result)
                         program error-output))
    output))

(defun %run-n3-once (directory btm run timeout-seconds)
  (let* ((command-timeout (min timeout-seconds 10d0))
         (started-at (get-internal-real-time))
         (output
           (%read-command-output btm '("--version") command-timeout))
         (elapsed-seconds
           (/ (- (get-internal-real-time) started-at)
              (float internal-time-units-per-second 1d0))))
    (when (zerop (length (string-trim '(#\Space #\Tab #\Return #\Newline)
                                      output)))
      (%measurement-fail "N3 ~a --version returned no output." btm))
    (%write-samples
     directory "n3" run
     (list (list :elapsed-seconds elapsed-seconds
                 :within-100ms-p (<= elapsed-seconds 0.1d0)
                 :version-output output)))))

(defun %run-n3 (directory btm rows columns rate runs duration timeout-seconds)
  (declare (ignore rows columns rate duration))
  (loop for run from 1 to runs
        do (%run-n3-once directory btm run timeout-seconds)))
(defun %split-whitespace (text)
  (let ((fields nil) (start nil))
    (loop for index from 0 to (length text)
          for separator = (or (= index (length text))
                              (find (char text index)
                                    '(#\Space #\Tab #\Return #\Newline #\{ #\})))
          do (cond
               ((and separator start)
                (push (subseq text start index) fields)
                (setf start nil))
               ((and (not separator) (null start))
                (setf start index))))
    (nreverse fields)))
(defun %parse-decimal (text)
  (let* ((text (string-trim '(#\Space #\Tab #\Return #\Newline) text))
         (point (position #\. text))
         (whole-text (if point (subseq text 0 point) text))
         (fraction-text (if point (subseq text (1+ point)) "")))
    (unless (and (plusp (length whole-text))
                 (every #'digit-char-p whole-text)
                 (every #'digit-char-p fraction-text))
      (%measurement-fail "Invalid decimal value: ~s" text))
    (+ (float (parse-integer whole-text) 1d0)
       (if (plusp (length fraction-text))
           (float (/ (parse-integer fraction-text)
                     (expt 10 (length fraction-text)))
                  1d0)
           0d0))))
(defun %parse-ps-cpu-duration (text)
  (let* ((text (string-trim '(#\Space #\Tab #\Return #\Newline) text))
         (dash (position #\- text))
         (days (if dash (parse-integer (subseq text 0 dash)) 0))
         (clock (if dash (subseq text (1+ dash)) text))
         (parts (loop with start = 0
                      for index from 0 to (length clock)
                      when (or (= index (length clock))
                               (char= (char clock index) #\:))
                        collect (subseq clock start index)
                        and do (setf start (1+ index))))
         (seconds
           (case (length parts)
             (2 (+ (* 60 (%parse-decimal (first parts)))
                   (%parse-decimal (second parts))))
             (3 (+ (* 3600 (%parse-decimal (first parts)))
                   (* 60 (%parse-decimal (second parts)))
                   (%parse-decimal (third parts))))
             (otherwise
              (%measurement-fail "Invalid ps CPU duration: ~s" text)))))
    (+ (* days 86400) seconds)))
(defun %parse-load-average (text)
  (let ((fields (%split-whitespace text)))
    (unless (= (length fields) 3)
      (%measurement-fail "Expected three load averages, got: ~s" text))
    (map 'vector #'%parse-decimal fields)))
(defun %darwin-required ()
  (unless (search "Darwin" (software-type) :test #'char-equal)
    (%measurement-unsupported
     "N1/N2 process-group accounting requires Darwin ps and sysctl.")))
(defun %integer-lines (text description)
  (let ((values
          (with-input-from-string (stream text)
            (loop for line = (read-line stream nil nil)
                  while line
                  for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
                  unless (zerop (length trimmed))
                    collect
                    (handler-case (parse-integer trimmed)
                      (error ()
                        (%measurement-fail "Invalid ~a line: ~s"
                                           description line)))))))
    (unless values (%measurement-fail "No ~a values were returned." description))
    values))
(defun %process-group-id (session timeout)
  (let ((ids (%integer-lines
              (%read-command-output
               "/bin/ps"
               (list "-o" "pgid=" "-p"
                     (write-to-string (e2e-session-pid session)))
               timeout)
              "process group id")))
    (unless (= (length ids) 1)
      (%measurement-fail "Expected one process group id, got ~d."
                         (length ids)))
    (first ids)))
(defun %process-group-cpu (group-id timeout)
  (let ((times
          (with-input-from-string
              (stream (%read-command-output
                       "/bin/ps"
                       (list "-o" "time=" "-g" (write-to-string group-id))
                       timeout))
            (loop for line = (read-line stream nil nil)
                  while line
                  for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
                  unless (zerop (length trimmed))
                    collect (%parse-ps-cpu-duration trimmed)))))
    (unless times
      (%measurement-fail "No CPU times were returned for process group ~d."
                         group-id))
    (values (reduce #'+ times) (length times))))
(defun %process-group-rss (group-id timeout)
  (let* ((text (%read-command-output
                "/bin/ps" (list "-o" "rss=" "-g" (write-to-string group-id))
                timeout))
         (rss (%integer-lines text "RSS")))
    (values (reduce #'+ rss) (length rss))))
(defun %system-load-average (timeout)
  (%parse-load-average
   (%read-command-output "/usr/sbin/sysctl" '("-n" "vm.loadavg") timeout)))
(defun %session-elapsed (session)
  (/ (- (get-internal-real-time) (e2e-session-started-at session))
     internal-time-units-per-second))
(defun %since (session start) (- (%session-elapsed session) start))
(defun %run-n1-once (directory btm rows columns rate duration timeout run)
  (%darwin-required)
  (let* ((config (%write-btm-config directory run rate))
         (session (%launch-btm btm config rows columns timeout))
         (samples nil))
    (unwind-protect
         (progn
           (multiple-value-bind (frame last-byte quiet)
               (%read-btm-frame session rate timeout)
             (declare (ignore last-byte quiet))
             (%write-octets
              (merge-pathnames (format nil "n1-run-~d-first.ansi" run)
                               directory)
              frame))
           (let ((start (%session-elapsed session))
                 (group-id (%process-group-id session timeout))
                 (previous-time nil)
                 (previous-cpu nil))
             (flet ((sample ()
                      (multiple-value-bind (cpu process-count)
                          (%process-group-cpu group-id timeout)
                        (let* ((elapsed (- (%session-elapsed session) start))
                               (load (%system-load-average timeout))
                               (percent
                                 (when (and previous-time previous-cpu
                                            (> elapsed previous-time))
                                   (* 100d0 (/ (- cpu previous-cpu)
                                               (- elapsed previous-time)))))
                               (record
                                 (list :elapsed-seconds elapsed
                                       :process-group group-id
                                       :process-count process-count
                                       :cpu-seconds cpu
                                       :cpu-percent percent
                                       :load-average load)))
                          (push record samples)
                          (setf previous-time elapsed previous-cpu cpu)))))
               (sample)
               (loop while (< (- (%session-elapsed session) start) duration)
                     do (multiple-value-bind (frame last-byte quiet)
                            (%read-btm-frame session rate timeout)
                          (declare (ignore frame last-byte quiet))
                          (sample)))))
           (%write-samples directory "n1" run (nreverse samples)))
      (close-e2e-session session))))
(defun %run-n1 (directory btm rows columns rate runs duration timeout)
  (loop for run from 1 to runs
        do (%run-n1-once directory btm rows columns rate duration timeout run)))

(defun %run-n2-once (directory btm rows columns rate duration timeout run)
  (%darwin-required)
  (let* ((config (%write-btm-config directory run rate))
         (session (%launch-btm btm config rows columns timeout))
         (samples nil))
    (unwind-protect
         (progn
           (multiple-value-bind (frame last-byte quiet)
               (%read-btm-frame session rate timeout)
             (declare (ignore last-byte quiet))
             (%write-octets
              (merge-pathnames (format nil "n2-run-~d-first.ansi" run)
                               directory)
              frame))
           (let ((start (%session-elapsed session))
                 (group-id (%process-group-id session timeout)))
             (flet ((sample ()
                      (multiple-value-bind (rss process-count)
                          (%process-group-rss group-id timeout)
                        (push (list :elapsed-seconds
                                    (- (%session-elapsed session) start)
                                    :process-group group-id
                                    :process-count process-count
                                    :rss-kib rss)
                              samples)))
                    (drain-to (target)
                      (loop while (< (- (%session-elapsed session) start)
                                     target)
                            do (multiple-value-bind (frame last-byte quiet)
                                   (%read-btm-frame session rate timeout)
                                 (declare (ignore frame last-byte quiet))))))
               (sample)
               (loop for target from *measurement-rss-cadence-seconds*
                       by *measurement-rss-cadence-seconds*
                     while (<= target duration)
                     do (drain-to target)
                        (sample))))
           (%write-samples directory "n2" run (nreverse samples)))
      (close-e2e-session session))))
(defun %run-n2 (directory btm rows columns rate runs duration timeout)
  (loop for run from 1 to runs
        do (%run-n2-once directory btm rows columns rate duration timeout run)))

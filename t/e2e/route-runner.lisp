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

(progn
  (defun %route-color-environment-variable-p (entry)
    (let ((separator (position #\= entry)))
      (and separator
           (member (subseq entry 0 separator)
                   '("ANSI_COLORS_DISABLED"
                     "CLICOLOR"
                     "CLICOLOR_FORCE"
                     "COLORTERM"
                     "FORCE_COLOR"
                     "NO_COLOR")
                   :test #'string=))))
  (progn
  ;; BTM's first macOS process sample took 26-47 seconds; allow one measured window.
  (defparameter *route-process-readiness-timeout-seconds* 60d0)
  (defparameter *route-process-readiness-sample-seconds* 1d0)
  (defun %route-isolated-environment-directory ()
    (let* ((directory (%route-temporary-directory "/tmp"))
           (claim (merge-pathnames ".claim" directory)))
      (when (probe-file claim)
        (delete-file claim))
      directory))
  (defun %route-environment
      (&optional (inherited-environment (sb-ext:posix-environ))
       (home-directory (%route-isolated-environment-directory))
       (config-directory (%route-isolated-environment-directory)))
    (append
     (list (format nil "HOME=~a"
                   (namestring (uiop:ensure-directory-pathname
                                (pathname home-directory))))
           (format nil "XDG_CONFIG_HOME=~a"
                   (namestring (uiop:ensure-directory-pathname
                                (pathname config-directory))))
           "TERM=xterm-256color")
     (remove-if
      (lambda (entry)
        (or (%route-color-environment-variable-p entry)
            (let ((separator (position #\= entry)))
              (and separator
                   (member (subseq entry 0 separator)
                           '("TERM" "HOME" "XDG_CONFIG_HOME")
                           :test #'string=)))))
      inherited-environment)))
  (defun %route-process-widget-ready-p (grid)
  (let* ((frames (and grid (criterion-projection-frames grid)))
         (frame
           (find-if (lambda (bounds)
                      (search "Processes"
                              (criterion-row-text grid (first bounds))
                              :test (function char-equal)))
                    frames))
         (header-row
           (and frame
                (loop for row from (1+ (first frame)) below (second frame)
                      when (criterion-process-header-row-p grid row (list frame))
                        return row)))
         (header-text (and header-row (criterion-row-text grid header-row)))
         (pid-span
           (and header-text
                (or (first (criterion-label-spans header-text "PID"))
                    (first (criterion-label-spans header-text "Count")))))
         (name-span
           (and header-text
                (first (criterion-label-spans header-text "Name(n)"))))
         (raw-name-end
           (and name-span
                (loop for label in (criterion-rule-values :process-header-labels)
                      for span = (first (criterion-label-spans header-text label))
                      when (and span (> (car span) (car name-span)))
                        minimize (car span))))
         (name-limit (or raw-name-end (and frame (fourth frame)))))
    (and frame header-row pid-span name-span name-limit
         (not (%route-widget-no-data-p grid "Processes"))
         (loop for row from (1+ header-row) below (second frame)
               for text = (criterion-row-text grid row)
               for pid-value = (subseq text (car pid-span) (car name-span))
               for name-value = (subseq text (car name-span) name-limit)
               thereis (and (find-if (function digit-char-p) pid-value)
                            (plusp (length
                                    (string-trim
                                     (list (code-char 32) (code-char 9)
                                           (code-char 9474))
                                     name-value))))))))
  (defun %route-read-interval (session raw seconds)
    (multiple-value-bind (deadline deadline-seconds)
        (%deadline-for-call session "process readiness" seconds)
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
            (query-scan-index (max 0 (- (length raw) 3))))
        (loop for now = (%now)
              for remaining = (%ticks-to-seconds (- deadline now))
              while (plusp remaining)
              do (when (with-e2e-operation-timeout
                        ("process readiness wait" deadline deadline-seconds)
                        (cl-tty-kit:fd-wait
                         (e2e-session-fd session) :input remaining))
                   (let ((count
                           (with-e2e-operation-timeout
                            ("process readiness read" deadline deadline-seconds)
                            (cl-tty-kit:fd-read-octets
                             (e2e-session-fd session) buffer))))
                     (cond
                       ((null count) nil)
                       ((zerop count)
                        (error "PTY closed during process readiness sampling."))
                       (t
                        (%append-octets raw (subseq buffer 0 count))
                        (%e2e-session-record-output session
                                                    (subseq buffer 0 count))
                        (multiple-value-bind (next-index)
                            (%pty-answer-cursor-query
                             session raw query-scan-index deadline)
                          (setf query-scan-index next-index)))))))
        raw)))
  (defun %route-wait-process-data (session raw rows columns)
    (incf (e2e-session-deadline session)
          (%seconds-to-ticks *route-process-readiness-timeout-seconds*))
    (incf (e2e-session-timeout-seconds session)
          *route-process-readiness-timeout-seconds*)
    (let ((deadline (+ (%now)
                       (%seconds-to-ticks
                        *route-process-readiness-timeout-seconds*)))
          (ready-samples 0))
      (loop for remaining =
              (%ticks-to-seconds (- deadline (%now)))
            while (plusp remaining)
            do (%route-read-interval
                session raw
                (min *route-process-readiness-sample-seconds* remaining))
               (if (%route-process-widget-ready-p
                    (%route-grid raw rows columns))
                   (when (= 2 (incf ready-samples))
                     (return-from %route-wait-process-data :ready))
                 (setf ready-samples 0))
            finally (return :timeout))))
  (progn
  (defun %route-process-data-required-p (route subcase)
    (let ((context (or (getf subcase :context) "")))
      (or (eq (route-kind route) :column)
          (string= (route-id route) "sleep-child")
          (string-equal context "normal process table")
          (equal (%route-widget-type context) "proc"))))
  (defun %route-process-startup-timeout (route subcase timeout)
    (if (%route-process-data-required-p route subcase)
        (max timeout *route-process-readiness-timeout-seconds*)
        timeout)))))

(defun %route-octets (text)
  (sb-ext:string-to-octets text :external-format :utf-8))
(defun %route-subcase-id (route index)
  (format nil "~a-~a~d"
          (route-id route)
          (if (< index 10) "00" (if (< index 100) "0" ""))
          index))
(defun %route-selected-routes (routes route-ids subcase-id)
  (let* ((unknown-route
           (find-if (lambda (route-id)
                      (not (find route-id routes
                                 :key (function route-id)
                                 :test (function string=))))
                    route-ids))
         (selected-routes
           (if route-ids
               (remove-if-not
                (lambda (route)
                  (member (route-id route) route-ids :test (function string=)))
                routes)
               routes))
         (subcase-route
           (when subcase-id
             (find-if
              (lambda (route)
                (loop for subcase in (%route-subcases-expanded route)
                      for index from 1
                      thereis
                      (string= subcase-id (%route-subcase-id route index))))
              routes))))
    (when unknown-route
      (error "Unknown route ID: ~a" unknown-route))
    (when (and subcase-id (null subcase-route))
      (error "Unknown subcase ID: ~a" subcase-id))
    (when (and subcase-route
               (not (member subcase-route selected-routes :test (function eq))))
      (error "Subcase ~a does not belong to the selected routes." subcase-id))
    (if subcase-route (list subcase-route) selected-routes)))
(defun %route-selected-subcases (route subcase-id)
  (loop for subcase in (%route-subcases-expanded route)
        for index from 1
        when (or (null subcase-id)
                 (string= subcase-id (%route-subcase-id route index)))
          collect (cons index subcase)))

(defun run-route-harness (&key (mode "ntop") (btm "btm") (ntop "./ntop")
                                   (evidence-directory "/tmp")
                                   (rows 60) (columns 220) (rate 250)
                                   (timeout-seconds 30) route-ids subcase-id)
  (unless (member mode '("ntop" "btm-self") :test (function string=))
    (error "Unknown route mode: ~s" mode))
  (let* ((directory (%route-temporary-directory evidence-directory))
         (config-path (merge-pathnames "btm-route-config.toml" directory))
         (report-path (merge-pathnames "report.md" directory))
         (all-routes (route-catalog))
         (routes (%route-selected-routes all-routes route-ids subcase-id))
         (results nil)
         (reference-program btm)
         (candidate-program (if (string= mode "btm-self") btm ntop)))
    (unless (= (length all-routes) 182)
      (error "Expected 182 routes, found ~d." (length all-routes)))
    (%write-text config-path (btm-config-string rate))
    (loop for route in routes
          for route-number from 1
          do (let* ((subcases (%route-selected-subcases route subcase-id))
                    (subresults nil)
                    (route-inputs nil))
               (cond
                 ((and (string= mode "btm-self")
                       (not (route-comparable-p route)))
                  (loop for indexed-subcase in subcases
                        for subcase-index = (car indexed-subcase)
                        for subcase = (cdr indexed-subcase)
                        do (push (%route-input-description route subcase)
                                 route-inputs)
                           (push (%route-noncomparable-manual-result
                                  route subcase subcase-index directory)
                                 subresults)))
                 ((route-manual-reason route)
                  (let ((subcase (cdar subcases)))
                    (push (%route-input-description route subcase) route-inputs)
                    (setf subresults
                          (list (%route-manual-result route subcase directory)))))
                 (t
                  (loop for indexed-subcase in subcases
                        for subcase-index = (car indexed-subcase)
                        for subcase = (cdr indexed-subcase)
                        do (push (%route-input-description route subcase)
                                 route-inputs)
                           (let ((manual-spec
                                   (%route-btm-self-manual-spec
                                    mode route subcase)))
                             (if manual-spec
                                 (let ((prefix
                                         (namestring
                                          (merge-pathnames
                                           (format nil "~a-~3,'0d"
                                                   (route-id route) subcase-index)
                                           directory))))
                                   (push (%route-manual-result
                                          route subcase directory
                                          (getf manual-spec :reason)
                                          prefix
                                          (getf manual-spec :procedure))
                                         subresults))
                                 (handler-case
                                     (push (%route-side-pair
                                            route subcase subcase-index mode
                                            reference-program candidate-program
                                            directory config-path rows columns
                                            rate timeout-seconds)
                                           subresults)
                                   (error (condition)
                                     (let* ((error-path
                                              (merge-pathnames
                                               (format nil "~a-~3,'0d-error.txt"
                                                       (route-id route)
                                                       subcase-index)
                                               directory))
                                            (frame-paths
                                              (loop for side in
                                                      '("reference" "candidate")
                                                    for frame-path =
                                                      (merge-pathnames
                                                       (format nil
                                                               "~a-~3,'0d-~a.ansi"
                                                               (route-id route)
                                                               subcase-index
                                                               side)
                                                       directory)
                                                    when (probe-file frame-path)
                                                      collect
                                                      (namestring frame-path))))
                                       (%write-text error-path
                                                    (princ-to-string condition))
                                       (push
                                        (list :passed nil
                                              :input
                                              (%route-input-description
                                               route subcase)
                                              :expected (route-expected route)
                                              :actual (princ-to-string condition)
                                              :cause
                                              "route execution raised an error"
                                              :first-difference nil
                                              :evidence
                                              (append
                                               frame-paths
                                               (list (namestring error-path))))
                                        subresults)))))))))
               (progn
                 (setf subresults (nreverse subresults))
                 (setf subresults
                       (loop for indexed-subcase in subcases
                             for subresult in subresults
                             collect (list* :subcase-id
                                            (%route-subcase-id
                                             route (car indexed-subcase))
                                            subresult))))
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
                          (symbol-name (%route-result-status route
                                                             subresults)))))))
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

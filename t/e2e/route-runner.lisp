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
  (defun %route-environment
      (&optional (inherited-environment (sb-ext:posix-environ)))
    (cons "TERM=xterm-256color"
          (remove-if
           (lambda (entry)
             (or (%route-color-environment-variable-p entry)
                 (and (>= (length entry) 5)
                      (string= entry "TERM=" :end1 5))))
           inherited-environment))))

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

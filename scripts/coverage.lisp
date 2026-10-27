(require "asdf")

(require "sb-cover")

(declaim (optimize sb-cover:store-coverage-data))

(defparameter *project-root*
  (truename
   (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *coverage-source-root*
  (merge-pathnames "src/" *project-root*))

(defparameter *coverage-report-directory*
  (uiop:ensure-directory-pathname
   (pathname
    (or (uiop:getenv "NTOP_COVERAGE_DIR")
        (merge-pathnames "ntop-coverage/" (uiop:temporary-directory))))))
(defparameter *coverage-compiled-source-count*
  0)

(asdf:load-asd (merge-pathnames "ntop.asd" *project-root*))

(asdf:load-system "cl-weave")

(progn
  (asdf:load-system "ntop")
  (cl-weave:reset-coverage)
  (let* ((system (asdf:find-system "ntop"))
         (output-root
           (uiop:ensure-directory-pathname
            (merge-pathnames "ntop-coverage-fasl/"
                             (uiop:temporary-directory))))
         (source-root-name (namestring *coverage-source-root*))
         (compiled-source-count 0))
    (dolist (component (asdf:component-children system))
      (let* ((source (asdf:component-pathname component))
             (source-name (namestring source)))
        (when (and (= 0 (or (search source-root-name source-name) -1))
                   (string= (pathname-type source) "lisp"))
          (let* ((relative (enough-namestring source *project-root*))
                 (output
                   (merge-pathnames
                    (make-pathname :type "fasl" :defaults (pathname relative))
                    output-root)))
            (ensure-directories-exist output)
            (multiple-value-bind (fasl-path warnings-p failure-p)
                (compile-file source :output-file output)
              (declare (ignore warnings-p))
              (when failure-p
                (error "Coverage compilation failed for ~A." source))
              (load fasl-path))
            (sb-cover:save-coverage)
            (incf compiled-source-count)))))
    (progn
      (unless (plusp compiled-source-count)
        (error "Coverage did not compile any source files."))
      (setf *coverage-compiled-source-count* compiled-source-count))))

(asdf:load-system "ntop/test" :force t)

(let* ((passed
        (cl-weave:run-all :reporter
                          :spec
                          :timeout-ms
                          40000
                          :max-workers
                          1
                          :pass-with-no-tests
                          nil
                          :coverage
                          t
                          :coverage-reset
                          nil
                          :coverage-include-pathnames
                          (list *coverage-source-root*)
                          :coverage-report-directory
                          (namestring *coverage-report-directory*))))
  (unless passed
    (error "The test suite contains failures."))
  (let* ((statistics
          (cl-weave:coverage-statistics :include-pathnames
                                        (list *coverage-source-root*)))
         (expression-covered (getf statistics :expression-covered))
         (expression-total (getf statistics :expression-total 0))
         (branch-covered (getf statistics :branch-covered))
         (branch-total (getf statistics :branch-total 0)))
    (when (or (zerop expression-total) (zerop branch-total))
      (error "Coverage did not measure executable expressions and branches."))
    (let ((minimum-coverage-percent 90d0)
          (expression-percent (* 100d0 (/ expression-covered expression-total)))
          (branch-percent (* 100d0 (/ branch-covered branch-total)))
          (below-threshold-files nil))
      (format t
              "Expression coverage: ~D/~D (~,1F%).~%"
              expression-covered
              expression-total
              expression-percent)
      (format t
              "Branch coverage: ~D/~D (~,1F%).~%"
              branch-covered
              branch-total
              branch-percent)
      (format t
              "Coverage report: ~A~%"
              (merge-pathnames "cover-index.html" *coverage-report-directory*))
      (let* ((coverage-file-statistics
              (cl-weave:coverage-file-statistics :include-pathnames
                                                 (list *coverage-source-root*)))
             (checked-file-count (length coverage-file-statistics)))
        (unless (= checked-file-count *coverage-compiled-source-count*)
          (error
           "Coverage statistics contain ~D files, but compiled ~D source files."
           checked-file-count
           *coverage-compiled-source-count*))
        (format t
                "Coverage files checked: ~D; compiled files: ~D.~%"
                checked-file-count
                *coverage-compiled-source-count*)
        (dolist (file-statistics coverage-file-statistics)
          (let* ((file-path
                  (enough-namestring (getf file-statistics :pathname)
                                     *project-root*))
                 (file-expression-covered
                  (getf file-statistics :expression-covered))
                 (file-expression-total
                  (getf file-statistics :expression-total))
                 (file-branch-covered (getf file-statistics :branch-covered))
                 (file-branch-total (getf file-statistics :branch-total)))
            (unless (plusp file-expression-total)
              (error "Coverage statistics contain no expressions for ~A."
                     file-path))
            (let ((file-expression-percent
                   (* 100d0 (/ file-expression-covered file-expression-total)))
                  (file-branch-percent
                   (when (plusp file-branch-total)
                     (* 100d0 (/ file-branch-covered file-branch-total)))))
              (format t
                      "COVERAGE_FILE|~A|~D|~D|~,1F|~D|~D|~A~%"
                      file-path
                      file-expression-covered
                      file-expression-total
                      file-expression-percent
                      file-branch-covered
                      file-branch-total
                      (if file-branch-percent
                          (format nil "~,1F" file-branch-percent)
                          "N/A"))
              (when
                  (or (< file-expression-percent minimum-coverage-percent)
                      (and file-branch-percent
                           (< file-branch-percent minimum-coverage-percent)))
                (push file-path below-threshold-files))))))
      (unless
          (and (>= expression-percent minimum-coverage-percent)
               (>= branch-percent minimum-coverage-percent))
        (error "Expression and branch coverage must both reach 90 percent."))
      (when below-threshold-files
        (error "Per-file expression and branch coverage must reach 90 percent: ~{~A~^, ~}"
               (nreverse below-threshold-files))))))

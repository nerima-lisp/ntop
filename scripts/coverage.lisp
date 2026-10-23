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

(asdf:load-asd (merge-pathnames "ntop.asd" *project-root*))

(asdf:load-system "cl-weave")

(progn
  (asdf:load-system "ntop" :force t)
  (asdf:load-system "ntop/test" :force t))

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
          (branch-percent (* 100d0 (/ branch-covered branch-total))))
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
      (dolist
          (file-statistics
           (cl-weave:coverage-file-statistics :include-pathnames
                                              (list *coverage-source-root*)))
        (let* ((file-expression-covered
                (getf file-statistics :expression-covered))
               (file-expression-total (getf file-statistics :expression-total))
               (file-expression-percent
                (* 100d0 (/ file-expression-covered file-expression-total)))
               (file-branch-covered (getf file-statistics :branch-covered))
               (file-branch-total (getf file-statistics :branch-total))
               (file-branch-percent
                (when (plusp file-branch-total)
                  (* 100d0 (/ file-branch-covered file-branch-total)))))
          (when
              (and file-branch-percent
                   (< file-branch-percent minimum-coverage-percent))
            (format t
                    (concatenate 'string
                                 "Branch coverage below 90%: ~A; "
                                 "expressions ~D/~D (~,1F%), "
                                 "branches ~D/~D (~,1F%).~%")
                    (enough-namestring (getf file-statistics :pathname)
                                       *project-root*)
                    file-expression-covered
                    file-expression-total
                    file-expression-percent
                    file-branch-covered
                    file-branch-total
                    file-branch-percent))))
      (unless
          (and (>= expression-percent minimum-coverage-percent)
               (>= branch-percent minimum-coverage-percent))
        (error "Expression and branch coverage must both reach 90 percent.")))))

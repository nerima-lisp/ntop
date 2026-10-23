(require :asdf)

(labels
    ((run-tests (on-success on-failure)
       (handler-case
           (let* ((script (or *load-truename*
                              *compile-file-truename*
                              (error "Unable to determine the script location.")))
                  (*default-pathname-defaults*
                    (uiop:pathname-directory-pathname script)))
             (asdf:load-asd (merge-pathnames "ntop.asd" script))
             (asdf:load-system "cl-weave")
             ;; cl-weave has no default timeout; fail a stalled test after 40 seconds.
             (setf (symbol-value
                    (uiop:find-symbol* (quote #:*default-timeout-ms*) (quote #:cl-weave)))
                   40000)
             (asdf:test-system "ntop")
             (funcall on-success))
         (error (condition)
           (funcall on-failure condition)))))
  (run-tests
   (lambda () (uiop:quit 0))
   (lambda (condition)
     (format *error-output* "Test run failed: ~A~%" condition)
     (uiop:quit 1))))

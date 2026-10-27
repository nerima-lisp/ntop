(progn
  (require "asdf")
  (let* ((script-path (truename *load-truename*))
         (project-root
           (truename
            (merge-pathnames "../"
                             (uiop:pathname-directory-pathname script-path))))
         (arguments (cdr sb-ext:*posix-argv*)))
    (when (> (length arguments) 5)
      (error "Expected at most five arguments: ntop, btm, table, rate, timeout."))
    (asdf:load-asd (merge-pathnames "ntop.asd" project-root))
    (asdf:load-system "ntop/e2e")
    (uiop:quit
     (ntop.e2e::run-condition-a-comparison
      :ntop (or (first arguments) "ntop")
      :btm (or (second arguments) "btm")
      :output (or (third arguments) "/tmp/ntop-fr001-condition-a.tsv")
      :rate (if (fourth arguments)
                (parse-integer (fourth arguments))
              700)
      :timeout-seconds
      (if (fifth arguments)
          (coerce (parse-integer (fifth arguments)) 'double-float)
        90d0)))))

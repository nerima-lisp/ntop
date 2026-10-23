(require "asdf")

(defparameter *project-root*
  (truename
   (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))

(asdf:load-asd (merge-pathnames "ntop.asd" *project-root*))

(let* ((system (asdf:find-system "ntop"))
       (operation (asdf:make-operation 'asdf:program-op))
       (output (asdf:output-file operation system)))
  (format t "NTOP_BUILD_OUTPUT=~A~%" output)
  (force-output)
  (asdf:make "ntop"))

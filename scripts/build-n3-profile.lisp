(require "asdf")

(defparameter *project-root*
  (truename
   (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))

(asdf:load-asd (merge-pathnames "ntop.asd" *project-root*))
(asdf:load-system "ntop")

(defun n3-monotonic-nanoseconds ()
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))

(defun n3-profile-marker (kind)
  (format *error-output*
          "N3_PROFILE_MARKER kind=~A clock=monotonic_ns value=~D~%"
          kind (n3-monotonic-nanoseconds))
  (force-output *error-output*))

(push (lambda () (n3-profile-marker "init")) sb-ext:*init-hooks*)
(push (lambda () (n3-profile-marker "exit")) sb-ext:*exit-hooks*)

(let* ((system (asdf:find-system "ntop"))
       (arguments (uiop:command-line-arguments))
       (output (if arguments
                   (merge-pathnames (first arguments) *project-root*)
                   (merge-pathnames "build/ntop-n3-profile" *project-root*))))
  (ensure-directories-exist output)
  (format t "NTOP_N3_PROFILE_OUTPUT=~A~%" output)
  (force-output)
  (sb-ext:save-lisp-and-die
   output
   :executable t
   :save-runtime-options t
   :toplevel (uiop:ensure-function
              (asdf/system:component-entry-point system))))

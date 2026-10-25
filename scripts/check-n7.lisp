(require "asdf")

(let* ((script-directory (uiop:pathname-directory-pathname *load-truename*))
       (root (truename (merge-pathnames "../" script-directory))))
  (asdf:load-asd (merge-pathnames "ntop.asd" root))
  (asdf:load-system "ntop/e2e")
  (uiop:symbol-call "NTOP.E2E" "RUN-N7-GENERIC" :root root)
  (format t "N7 generic scan passed.~%"))

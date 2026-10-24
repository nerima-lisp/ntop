(in-package #:ntop.e2e)

(progn
  (in-package #:ntop.e2e)

  (defun %route-cli-output (side)
    (format nil "~a~%~a" (getf side :stdout) (getf side :stderr)))

  (defun %route-output-has-digit-p (text)
    (find-if #'digit-char-p text))

  (defun %route-control-output-valid-p (route program side)
    (let* ((output (%route-cli-output side))
           (name (pathname-name (pathname program)))
           (kind (route-action route)))
      (and (eql (getf side :exit-code) 0)
           (case (intern (string-upcase kind) :keyword)
             (:HELP
              (and (search "Usage:" output)
                   (search "Options:" output)
                   (search name output :test #'char-equal)))
             (:VERSION
              (and (search (if (string= name "btm") "bottom" "ntop") output
                           :test #'char-equal)
                   (%route-output-has-digit-p output)))
             (otherwise nil)))))

  (defun %route-run-invalid-flag-pair
    (route description reference-args candidate-args mode btm ntop
     reference-prefix candidate-prefix timeout)
  (let* ((reference (%route-run-cli-side btm reference-args reference-prefix
                                         timeout))
         (candidate (%route-run-cli-side ntop candidate-args candidate-prefix
                                         timeout))
         (reference-code (getf reference :exit-code))
         (candidate-code (getf candidate :exit-code))
         (reference-output (%route-cli-output reference))
         (candidate-output (%route-cli-output candidate))
         (rejected-p (and (integerp reference-code)
                          (integerp candidate-code)
                          (not (zerop reference-code))
                          (not (zerop candidate-code))
                          (= reference-code candidate-code)))
         (equal-p (string= reference-output candidate-output))
         (passed (and rejected-p
                      (or (not (string= mode "btm-self")) equal-p))))
    (list :passed passed
          :input description
          :expected (route-expected route)
          :actual (format nil "btm exit ~a: ~a; candidate exit ~a: ~a"
                          reference-code
                          (string-trim '(#\Space #\Newline #\Return)
                                       reference-output)
                          candidate-code
                          (string-trim '(#\Space #\Newline #\Return)
                                       candidate-output))
          :cause (cond ((not rejected-p)
                        "invalid flag was not rejected with matching nonzero exits")
                       ((and (string= mode "btm-self") (not equal-p))
                        "btm-self invalid-flag output differs")
                       (t "both CLIs rejected the invalid flag with matching status"))
          :first-difference nil
          :evidence (mapcar #'namestring
                            (list (getf reference :evidence)
                                  (getf candidate :evidence))))))
)

(in-package #:ntop.e2e)

(defun %route-run-cli-side (program arguments evidence-prefix timeout)
  (let* ((result (%route-parse-output program arguments timeout))
         (path (merge-pathnames (format nil "~a.txt" evidence-prefix)))
         (output (format nil "stdout:~%~a~%stderr:~%~a"
                         (getf result :stdout) (getf result :stderr))))
    (%write-text path output)
    (list :exit-code (getf result :exit-code)
          :actual (format nil "exit ~a; ~a"
                          (getf result :exit-code)
                          (string-trim '(#\Space #\Newline #\Return)
                                       (getf result :stdout)))
          :stdout (getf result :stdout)
          :stderr (getf result :stderr)
          :output (concatenate 'string (getf result :stdout)
                               (getf result :stderr))
          :evidence path)))

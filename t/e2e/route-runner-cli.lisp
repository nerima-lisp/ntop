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
    (let* ((reference
            (%route-capture-side :reference #'%route-run-cli-side btm
                                 reference-args reference-prefix timeout))
           (candidate
            (%route-capture-side :candidate #'%route-run-cli-side ntop
                                 candidate-args candidate-prefix timeout))
           (reference-code (getf reference :exit-code))
           (candidate-code (getf candidate :exit-code))
           (reference-output (%route-cli-output reference))
           (candidate-output (%route-cli-output candidate))
           (rejected-p (and (integerp reference-code)
                            (integerp candidate-code)
                            (not (zerop reference-code))
                            (not (zerop candidate-code))
                            (= reference-code candidate-code)))
           (equal-p (and (eql reference-code candidate-code)
                         (string= reference-output candidate-output)))
           (passed (and rejected-p
                        (or (not (string= mode "btm-self")) equal-p)))
           (difference (when (and (string= mode "btm-self") (not equal-p))
                         "btm-self invalid-flag exit status or output differs")))
      (list :passed passed
            :classification
            (unless passed
              (cond (difference :semantic-mismatch)
                    ((not (and (integerp candidate-code)
                               (not (zerop candidate-code))))
                     :candidate-failure)
                    (t :assertion-failure)))
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
                         (difference difference)
                         (t "both CLIs rejected the invalid flag with matching status"))
            :first-difference difference
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
(defun %route-run-config-location-pair
    (route subcase description btm ntop reference-prefix candidate-prefix
           candidate-args rows columns rate timeout config-path evidence-directory)
  (let* ((reference
           (%route-capture-side
            :reference (function %route-run-interactive-side) btm route subcase
            reference-prefix rows columns rate timeout config-path
            evidence-directory))
         (candidate
           (%route-capture-side
            :candidate (function %route-run-cli-side) ntop candidate-args
            candidate-prefix timeout))
         (candidate-output (%route-cli-output candidate))
         (candidate-valid-p
           (and (eql (getf candidate :exit-code) 2)
                (search "invalid command-line input" candidate-output
                        :test (function char-equal))))
         (passed candidate-valid-p))
    (setf (getf reference :passed) t
          (getf candidate :passed) candidate-valid-p)
    (%route-paired-result
     description
     "BTM captures a TUI frame from config_location; ntop exits 2 with invalid command-line input"
     passed
     (format nil "btm TUI: ~a; candidate CLI: ~a"
             (getf reference :actual) (getf candidate :actual))
     (if candidate-valid-p
         "BTM accepted the configured path and ntop reported the expected CLI error"
         "ntop CLI did not exit 2 with the expected invalid command-line input message")
     nil reference candidate)))

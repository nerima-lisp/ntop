(in-package #:ntop.e2e.test)

(defun criterion-a-capture-test-script (post-query)
  (format nil
          (concatenate 'string
                       "import os,time~%"
                       "def top(title):~%"
                       "    return \"┌─\"+title+\"─\"*(40-len(title)-3)+\"┐\"~%"
                       "def inside(value):~%"
                       "    return \"│\"+value.ljust(38)[:38]+\"│\"~%"
                       "bottom=\"└\"+\"─\"*38+\"┘\"~%"
                       "lines=[top(\"CPU\"),inside(\"A\"*38),inside(\"\"),~%"
                       "       inside(\"\"),inside(\"\"),bottom,~%"
                       "       top(\"Processes\"),inside(\"Count  Name(n)\"),~%"
                       "       inside(\"1  python\"),inside(\"\"),~%"
                       "       inside(\"\"),bottom]~%"
                       "time.sleep(0.04)~%"
                       "for index,line in enumerate(lines,1):~%"
                       "    os.write(1,f\"\\x1b[{index};1H{line}\".encode())~%"
                       "os.write(1,b'\\x1b[6n')~%"
                       "~a")
          (format nil post-query)))

(defun criterion-a-capture-test-raw-path (kind)
  (merge-pathnames
   (format nil "ntop-condition-a-~a-~a.ansi" kind (gensym))
   (uiop:temporary-directory)))

(cl-weave:describe
 "Condition A frame capture"
 (cl-weave:it
  "starts the stability window after CPR when no later output arrives"
  (let* ((script
           (criterion-a-capture-test-script
            "while True: time.sleep(1)~%"))
         (raw-path (criterion-a-capture-test-raw-path "quiet"))
         (started-at (ntop.e2e::%now))
         (grid nil)
         (metadata nil))
    (unwind-protect
         (let ((ntop.e2e::*route-frame-stability-seconds* 0.08d0))
           (multiple-value-setq (grid metadata)
             (ntop.e2e::%condition-a-capture
              "python3" (list "-c" script) 12 40 1.5d0 nil raw-path))
           (cl-weave:expect
            (ntop.e2e::criterion-projection-frames grid)
            :to-equal '((0 5 0 39) (6 11 0 39)))
           (cl-weave:expect
            (and
             (>= (ntop.e2e::%ticks-to-seconds
                  (- (ntop.e2e::%now) started-at))
                 0.08d0)
             (not (getf metadata :awaiting-post-query-output-p))
             (getf metadata :candidate-grid-p)
             (realp (getf metadata :stable-check-at-seconds))
             (realp (getf metadata :last-output-seconds))
             (>= (- (getf metadata :stable-check-at-seconds)
                    (getf metadata :last-output-seconds))
                 0.08d0))
            :to-be t)
           (cl-weave:expect
            (probe-file raw-path)
            :to-satisfy (function identity)))
      (when (probe-file raw-path)
        (delete-file raw-path))))))
 (cl-weave:describe
 "Condition A incomplete CSI frame"
 (cl-weave:it
  "keeps Condition A stable when output ends in an incomplete CSI"
  (let* ((tail
           (concatenate 'string
                        "for value in range(10):~%"
                        "    os.write(1,f\"\\x1b[2;2H{chr(65+value%2)}\".encode())~%"
                        "    time.sleep(0.02)~%"
                        "os.write(1,bytes((27,91)))~%"
                        "while True: time.sleep(1)~%"))
         (script (criterion-a-capture-test-script tail))
         (raw-path (criterion-a-capture-test-raw-path "dynamic"))
         (started-at (ntop.e2e::%now))
         (grid nil)
         (captured nil))
    (unwind-protect
         (let ((ntop.e2e::*route-frame-stability-seconds* 0.35d0))
           (setf grid
                 (ntop.e2e::%condition-a-capture
                  "python3" (list "-c" script) 12 40 2d0 nil raw-path))
           (setf captured
                 (with-open-file
                     (stream raw-path :direction :input
                                      :element-type '(unsigned-byte 8))
                   (let ((octets
                           (make-array (file-length stream)
                                       :element-type '(unsigned-byte 8))))
                     (read-sequence octets stream)
                     octets)))
           (cl-weave:expect
            (ntop.e2e::criterion-projection-frames grid)
            :to-equal '((0 5 0 39) (6 11 0 39)))
           (cl-weave:expect
            (and
             (search (ntop.e2e::%route-octets
                      (format nil "~c[2;2HA" #\Escape))
                     captured :test (function =))
             (search (ntop.e2e::%route-octets
                      (format nil "~c[2;2HB" #\Escape))
                     captured :test (function =))
             (equalp (subseq captured (- (length captured) 2)) #(27 91)))
            :to-be-truthy)
           (cl-weave:expect
            (< (ntop.e2e::%ticks-to-seconds
                (- (ntop.e2e::%now) started-at))
               1.5d0)
            :to-be t))
      (when (probe-file raw-path)
        (delete-file raw-path))))))
 (cl-weave:describe
 "Condition A capture side attribution"
 (cl-weave:it
  "records reference and candidate sides on capture timeouts"
  (let* ((symbols
           (list (find-symbol "%CONDITION-A-CAPTURE" "NTOP.E2E")
                 (find-symbol "GOLDEN-DIMENSIONS" "NTOP.E2E")
                 (find-symbol "%MAKE-E2E-TEMPORARY-DIRECTORY" "NTOP.E2E")))
         (original-functions
           (mapcar (lambda (symbol) (symbol-function symbol)) symbols)))
    (dolist (timeout-side (list :reference :candidate))
      (let* ((directory (funcall (third original-functions)))
             (output
               (merge-pathnames
                (format nil "comparison-~(~a~).tsv" timeout-side)
                directory))
             (condition nil))
        (unwind-protect
             (progn
               (setf (symbol-function (second symbols))
                     (lambda () (list (list 40 12))))
               (setf (symbol-function (third symbols))
                     (lambda () directory))
               (setf (symbol-function (first symbols))
                     (lambda (program arguments rows columns timeout-seconds
                              environment raw-path)
                       (declare
                        (ignore program arguments timeout-seconds environment
                                raw-path))
                       (if (eq ntop.e2e::*route-frame-capture-side*
                               timeout-side)
                           (let ((session
                                   (ntop.e2e::launch-e2e-process
                                    "python3"
                                    (list "-c" "import time; time.sleep(30)")
                                    rows columns :timeout-seconds 2d0)))
                             (unwind-protect
                                  (ntop.e2e::%route-capture-frame
                                   session
                                   (make-array
                                    0 :element-type '(unsigned-byte 8))
                                   rows columns
                                   :stage :condition-a-side-attribution
                                   :timeout-seconds 0.15d0
                                   :stability-seconds 0.05d0)
                               (ntop.e2e::close-e2e-session session)))
                           (ntop.e2e:make-terminal-grid
                            :rows rows :cols columns))))
               (setf condition
                     (handler-case
                         (progn
                           (ntop.e2e::run-condition-a-comparison
                            :output output :rate 700 :timeout-seconds 2d0)
                           nil)
                       (ntop.e2e::route-frame-capture-timeout (caught)
                         caught)))
               (cl-weave:expect
                (typep condition
                       (find-symbol "ROUTE-FRAME-CAPTURE-TIMEOUT" "NTOP.E2E"))
                :to-be t)
               (cl-weave:expect
                (typep condition (find-symbol "E2E-TIMEOUT" "NTOP.E2E"))
                :to-be t)
               (cl-weave:expect
                (ntop.e2e::route-frame-timeout-side condition)
                :to-equal timeout-side)
               (cl-weave:expect
                (ntop.e2e::route-frame-timeout-stage condition)
                :to-equal :condition-a-side-attribution)
               (cl-weave:expect
                (> (ntop.e2e::route-frame-timeout-elapsed-seconds condition) 0)
                :to-be t)
               (let ((report (format nil "~a" condition)))
                 (cl-weave:expect
                  (search (string-downcase (symbol-name timeout-side))
                          (string-downcase report))
                  :to-satisfy (function identity))))
          (loop for symbol in symbols
                for original in original-functions
                do (setf (symbol-function symbol) original))
          (uiop:delete-directory-tree directory :validate t)))))))

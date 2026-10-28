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
        (delete-file raw-path)))))

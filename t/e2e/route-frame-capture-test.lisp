(in-package #:ntop.e2e.test)
(cl-weave:describe
 "route frame compatibility"
 (cl-weave:it
  "reports a timeout when no initial frame is available"
  (let* ((session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" "import time; time.sleep(30)") 24 80
            :timeout-seconds 3d0))
         (caught nil))
    (unwind-protect
         (progn
           (handler-case
               (let ((ntop.e2e::*route-frame-capture-not-before-at*
                         (+ (ntop.e2e::%now)
                            (ntop.e2e::%seconds-to-ticks 0.08d0))))
                   (ntop.e2e::%route-capture-frame
                    session
                    (route-frame-capture-test-empty-octets)
                    24 80
                    :side :candidate
                    :stage :initial-frame-required
                    :timeout-seconds 0.5d0
                    :stability-seconds 0.08d0))
             (ntop.e2e::route-frame-capture-timeout (condition)
               (setf caught condition)))
           (cl-weave:expect
            (typep caught 'ntop.e2e::route-frame-capture-timeout)
            :to-be t)
           (cl-weave:expect
            (ntop.e2e::route-frame-timeout-side caught)
            :to-be :candidate)
           (cl-weave:expect
            (ntop.e2e::route-frame-timeout-stage caught)
            :to-be :initial-frame-required)
           (cl-weave:expect
            (and (realp
                  (ntop.e2e::route-frame-timeout-elapsed-seconds caught))
                 (> (ntop.e2e::route-frame-timeout-elapsed-seconds caught) 0)
                 (null
                  (ntop.e2e::route-frame-timeout-last-output-seconds caught))
                 (null
                  (ntop.e2e::route-frame-timeout-stable-signature caught)))
            :to-be t)
           (let ((report (format nil "~a" caught)))
             (cl-weave:expect
              (every (lambda (label) (search label report))
                     (list "Side:" "stage:" "elapsed:" "last output:"
                           "last stable signature:"))
              :to-be t))
           (cl-weave:expect
            (sb-ext:process-alive-p (ntop.e2e::e2e-session-process session))
            :to-be t))
      (ntop.e2e::close-e2e-session session)))))
(cl-weave:describe
 "route quiet period compatibility"
(cl-weave:it
  "waits for a delayed border cell after an early completion predicate"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,bytes([27])+b'[1;1HREADY')~%"
                                "time.sleep(0.40)~%"
                                "os.write(1,bytes([27])+b'[1;12H|')~%"
                                "while True: time.sleep(1)~%")))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 2 12 :timeout-seconds 3d0))
         (started nil)
         (output nil))
    (unwind-protect
         (progn
           (cl-weave:expect
            (cl-tty-kit:fd-wait (ntop.e2e::e2e-session-fd session) :input 2d0)
            :to-be t)
           (setf started (ntop.e2e::%now))
           (setf output
                 (let ((ntop.e2e::*route-frame-capture-not-before-at*
                         (+ (ntop.e2e::%now)
                            (ntop.e2e::%seconds-to-ticks 0.05d0))))
                   (ntop.e2e::%route-capture-frame
                    session
                    (route-frame-capture-test-empty-octets)
                    2 12
                    :timeout-seconds 2d0
                    :stability-seconds 0.25d0
                    :completion-predicate
                    (lambda (grid)
                      (char=
                       (ntop.e2e:cell-character
                        (ntop.e2e:grid-cell-at grid 0 11))
                       #\|)))))
           (let ((grid (ntop.e2e::%route-grid output 2 12)))
             (cl-weave:expect
              (char-code
               (ntop.e2e:cell-character
                (ntop.e2e:grid-cell-at grid 0 11)))
              :to-equal
              124)
             (cl-weave:expect
              (>= (ntop.e2e::%ticks-to-seconds
                   (- (ntop.e2e::%now) started))
                  0.55d0)
              :to-be
              t)))
      (ntop.e2e::close-e2e-session session))))
(cl-weave:it
  "honors the initial quiet deadline with an early completion predicate"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,bytes([27])+b'[1;1HREADY')~%"
                                "while True: time.sleep(1)~%")))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 2 12 :timeout-seconds 3d0))
         (started nil)
         (output nil))
    (unwind-protect
         (progn
           (cl-weave:expect
            (cl-tty-kit:fd-wait (ntop.e2e::e2e-session-fd session) :input 2d0)
            :to-be t)
           (setf started (ntop.e2e::%now))
           (setf output
                 (let ((ntop.e2e::*route-frame-capture-not-before-at*
                         (+ (ntop.e2e::%now)
                            (ntop.e2e::%seconds-to-ticks 0.40d0))))
                   (ntop.e2e::%route-capture-frame
                    session
                    (route-frame-capture-test-empty-octets)
                    2 12
                    :timeout-seconds 2d0
                    :stability-seconds 0.10d0
                    :completion-predicate
                    (lambda (grid)
                      (char=
                       (ntop.e2e:cell-character
                        (ntop.e2e:grid-cell-at grid 0 0))
                       #\R)))))
           (let ((grid (ntop.e2e::%route-grid output 2 12)))
             (cl-weave:expect
              (char-code
               (ntop.e2e:cell-character
                (ntop.e2e:grid-cell-at grid 0 0)))
              :to-equal
              82)
             (cl-weave:expect
              (>= (ntop.e2e::%ticks-to-seconds
                   (- (ntop.e2e::%now) started))
                  0.30d0)
              :to-be
              t)))
      (ntop.e2e::close-e2e-session session))))
)

(defun route-frame-capture-test-launch (script rows columns)
  (ntop.e2e::launch-e2e-process
   "python3" (list "-c" script) rows columns :timeout-seconds 4d0))

(defun route-frame-capture-test-empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(cl-weave:describe
 "route frame capture engine"
 (cl-weave:it
  "accumulates split CSI and UTF-8 output through a complete grid"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,bytes([27,91,50,59]))~%"
                                "time.sleep(0.04)~%"
                                "os.write(1,bytes([52,72,206]))~%"
                                "time.sleep(0.04)~%"
                                "os.write(1,bytes([187]))~%"
                                "while True: time.sleep(1)~%")))
         (session (route-frame-capture-test-launch script 3 12)))
    (unwind-protect
         (multiple-value-bind (octets grid metadata)
             (let ((ntop.e2e::*route-frame-stability-seconds* 0.12d0))
               (ntop.e2e::%route-capture-frame
                session (route-frame-capture-test-empty-octets) 3 12
                :side :candidate :stage :fragmented-ansi
                :timeout-seconds 1.5d0))
           (cl-weave:expect
            (coerce octets 'list) :to-equal
            '(#x1b #x5b #x32 #x3b #x34 #x48 #xce #xbb))
           (cl-weave:expect
            (ntop.e2e:cell-character (ntop.e2e:grid-cell-at grid 1 3))
            :to-equal #\λ)
           (cl-weave:expect
            (and (eq (getf metadata :side) :candidate)
                 (= ntop.e2e::*route-frame-stability-seconds* 3.3d0))
            :to-be t)
           (cl-weave:expect (getf metadata :stage) :to-be :fragmented-ansi)
           (cl-weave:expect
            (realp (getf metadata :elapsed-seconds)) :to-be t)
           (cl-weave:expect
            (realp (getf metadata :last-output-seconds)) :to-be t)
           (cl-weave:expect
            (getf metadata :stable-signature) :to-satisfy (function identity)))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "waits for a delayed right border cell after the predicate becomes true"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,b'\\x1b[1;1HREADY')~%"
                                "time.sleep(0.12)~%"
                                "os.write(1,bytes([27,91,49,59,49,50,72,124]))~%"
                                "while True: time.sleep(1)~%")))
         (session (route-frame-capture-test-launch script 2 12))
         (predicate-received-grid-p nil)
         (ready-before-border-p nil))
    (unwind-protect
         (multiple-value-bind (octets grid metadata)
             (ntop.e2e::%route-capture-frame
              session (route-frame-capture-test-empty-octets) 2 12
              :side :reference :stage :delayed-border
              :timeout-seconds 1.5d0 :stability-seconds 0.18d0
              :completion-predicate
              (lambda (current-grid)
                (setf predicate-received-grid-p
                      (typep current-grid 'ntop.e2e::terminal-grid))
                (let ((ready-p
                        (and (char= (ntop.e2e:cell-character
                                     (ntop.e2e:grid-cell-at current-grid 0 0))
                                    #\R)
                             (char= (ntop.e2e:cell-character
                                     (ntop.e2e:grid-cell-at current-grid 0 4))
                                    #\Y))))
                  (when (and ready-p
                             (char= (ntop.e2e:cell-character
                                     (ntop.e2e:grid-cell-at current-grid 0 11))
                                    #\Space))
                    (setf ready-before-border-p t))
                  ready-p)))
           (declare (ignore octets metadata))
           (cl-weave:expect predicate-received-grid-p :to-be t)
           (cl-weave:expect ready-before-border-p :to-be t)
           (cl-weave:expect
            (ntop.e2e:cell-character (ntop.e2e:grid-cell-at grid 0 11))
            :to-equal #\|))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "returns a stable blank pre-action grid when an action produces no output"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,bytes((27,91,50,74,27,91,72)))~%"
                                "while True: time.sleep(1)~%")))
         (session (route-frame-capture-test-launch script 3 12)))
    (unwind-protect
         (multiple-value-bind (prefix before-grid before-metadata)
             (ntop.e2e::%route-capture-frame
              session (route-frame-capture-test-empty-octets) 3 12
              :side :candidate :stage :pre-action
              :timeout-seconds 1.5d0 :stability-seconds 0.08d0)
           (let ((started-at (ntop.e2e::%now)))
             (let ((ntop.e2e::*route-frame-capture-not-before-at*
                     (+ (ntop.e2e::%now)
                        (ntop.e2e::%seconds-to-ticks 0.20d0))))
               (multiple-value-bind (after-prefix after-grid after-metadata)
                   (ntop.e2e::%route-capture-frame
                    session prefix 3 12
                    :side :candidate :stage :post-action
                    :timeout-seconds 0.8d0 :stability-seconds 0.08d0
                    :allow-unchanged-p t)
                 (declare (ignore after-metadata))
                 (cl-weave:expect (equalp after-prefix prefix) :to-be t)
                 (cl-weave:expect
                  (ntop.e2e:cell-character
                   (ntop.e2e:grid-cell-at before-grid 0 0))
                  :to-equal #\Space)
                 (cl-weave:expect
                  (ntop.e2e:cell-character
                   (ntop.e2e:grid-cell-at after-grid 0 0))
                  :to-equal #\Space)
                 (cl-weave:expect
                  (getf before-metadata :stable-signature)
                  :to-be-truthy)
                 (cl-weave:expect
                  (>= (ntop.e2e::%ticks-to-seconds
                       (- (ntop.e2e::%now) started-at))
                      0.15d0)
                  :to-equal t)))))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "uses one deadline and reports capture diagnostics while output continues"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,b'\\x1b[1;1HBASE')~%"
                                "time.sleep(0.18)~%"
                                "while True:~%"
                                "    os.write(1,b'\\x1b[1;1HB')~%"
                                "    time.sleep(0.04)~%")))
         (session (route-frame-capture-test-launch script 3 12))
         (captured nil)
         (condition nil))
    (unwind-protect
         (progn
           (multiple-value-bind (prefix grid metadata)
               (ntop.e2e::%route-capture-frame
                session (route-frame-capture-test-empty-octets) 3 12
                :side :reference :stage :timeout-baseline
                :timeout-seconds 1d0 :stability-seconds 0.08d0)
             (declare (ignore grid metadata))
             (setf captured prefix))
           (handler-case
               (ntop.e2e::%route-capture-frame
                session captured 3 12
                :side :reference :stage :deadline-probe
                :timeout-seconds 0.32d0 :stability-seconds 0.12d0
                :completion-predicate (lambda (grid)
                                        (declare (ignore grid))
                                        nil))
             (ntop.e2e::route-frame-capture-timeout (caught)
               (setf condition caught)))
           (cl-weave:expect
            (typep condition 'ntop.e2e::route-frame-capture-timeout)
            :to-be t)
           (cl-weave:expect
            (typep condition 'ntop.e2e::e2e-timeout) :to-be t)
           (cl-weave:expect
            (ntop.e2e::route-frame-timeout-side condition)
            :to-be :reference)
           (cl-weave:expect
            (ntop.e2e::route-frame-timeout-stage condition)
            :to-be :deadline-probe)
           (cl-weave:expect
            (<= 0.2d0
                (ntop.e2e::route-frame-timeout-elapsed-seconds condition)
                1d0)
            :to-be t)
           (cl-weave:expect
            (realp
             (ntop.e2e::route-frame-timeout-last-output-seconds condition))
            :to-be t)
           (cl-weave:expect
            (ntop.e2e::route-frame-timeout-stable-signature condition)
            :to-satisfy (function identity))
           (let ((report (princ-to-string condition)))
             (dolist (needle
                      '("reference" "deadline-probe" "elapsed"
                        "last" "output" "signature"))
               (cl-weave:expect
                (not (null (search needle report :test (function char-equal))))
                :to-be t))))
      (ntop.e2e::close-e2e-session session)))))

(defun route-frame-capture-test-timeout (script stage)
  (let ((session (route-frame-capture-test-launch script 2 12)))
    (unwind-protect
         (handler-case
             (progn
               (ntop.e2e::%route-capture-frame
                session (route-frame-capture-test-empty-octets) 2 12
                :side :reference :stage stage
                :timeout-seconds 0.75d0 :stability-seconds 0.08d0)
               nil)
           (ntop.e2e::route-frame-capture-timeout (condition)
             condition))
      (ntop.e2e::close-e2e-session session))))

(cl-weave:describe
 "route frame capture cursor query"
 (cl-weave:it
  "starts the stability window when it answers a quiet CPR query"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,bytes((27,91,50,74,27,91,72))"
                                "+b'READY'+bytes((27,91,54,110)))~%"
                                "while True: time.sleep(1)~%")))
         (session (route-frame-capture-test-launch script 2 12))
         (started-at (ntop.e2e::%now)))
    (unwind-protect
         (multiple-value-bind (octets grid metadata)
             (ntop.e2e::%route-capture-frame
              session (route-frame-capture-test-empty-octets) 2 12
              :side :reference :stage :quiet-cursor-query
              :timeout-seconds 1d0 :stability-seconds 0.08d0)
           (cl-weave:expect
            (search #(27 91 54 110) octets :test (function =))
            :to-be-truthy)
           (cl-weave:expect
            (ntop.e2e:cell-character (ntop.e2e:grid-cell-at grid 0 0))
            :to-equal #\R)
           (cl-weave:expect
            (and
             (not (ntop.e2e::e2e-session-awaiting-post-query-output-p session))
             (getf metadata :candidate-grid-p)
             (realp (getf metadata :stable-check-at-seconds)))
            :to-be t)
           (cl-weave:expect
            (>= (- (getf metadata :stable-check-at-seconds)
                   (getf metadata :last-output-seconds))
                0.08d0)
            :to-be t)
           (cl-weave:expect
            (>= (ntop.e2e::%ticks-to-seconds
                 (- (ntop.e2e::%now) started-at))
                0.08d0)
            :to-be t))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "reports an incomplete CSI when no candidate grid can be built"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,bytes((27,91,50,74,27,91,72))+b'FRAME'"
                                "+bytes((27,91)))~%"
                                "while True: time.sleep(1)~%")))
         (condition
           (route-frame-capture-test-timeout script :incomplete-csi)))
    (cl-weave:expect
     (typep condition 'ntop.e2e::route-frame-capture-timeout)
     :to-be t)
    (cl-weave:expect
     (and
      (not (ntop.e2e::route-frame-timeout-candidate-grid-p condition))
      (null (ntop.e2e::route-frame-timeout-stable-check-at-seconds condition))
      (eq (ntop.e2e::route-frame-timeout-grid-unavailable-reason condition)
          :incomplete-escape-suffix))
     :to-be t)
    (cl-weave:expect
     (search "grid unavailable: :INCOMPLETE-ESCAPE-SUFFIX."
             (format nil "~a" condition))
     :to-be-truthy)))
 (cl-weave:it
  "reports an incomplete UTF-8 suffix when no candidate grid can be built"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,bytes((27,91,50,74,27,91,72))+b'FRAME'"
                                "+bytes((206,)))~%"
                                "while True: time.sleep(1)~%")))
         (condition
           (route-frame-capture-test-timeout script :incomplete-utf8)))
    (cl-weave:expect
     (typep condition 'ntop.e2e::route-frame-capture-timeout)
     :to-be t)
    (cl-weave:expect
     (and
      (not (ntop.e2e::route-frame-timeout-candidate-grid-p condition))
      (null (ntop.e2e::route-frame-timeout-stable-check-at-seconds condition))
      (eq (ntop.e2e::route-frame-timeout-grid-unavailable-reason condition)
          :incomplete-utf8-suffix))
     :to-be t)
    (cl-weave:expect
     (search "grid unavailable: :INCOMPLETE-UTF8-SUFFIX."
             (format nil "~a" condition))
     :to-be-truthy)))
 )
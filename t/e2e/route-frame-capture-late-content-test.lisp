(in-package #:ntop.e2e.test)

(cl-weave:describe
 "late panel completion after stability"
 (cl-weave:it
  "reevaluates completion after stable panel content changes"
  (let* ((script
          (format nil
                  (concatenate 'string
                               "import os,time~%"
                               "top=(chr(0x250c)+chr(0x2500)*10+chr(0x2510)).encode()~%"
                               "middle=(chr(0x2502)+chr(32)*10+chr(0x2502)).encode()~%"
                               "bottom=(chr(0x2514)+chr(0x2500)*10+chr(0x2518)).encode()~%"
                               "os.write(1,bytes([27,91,49,59,50,72])+top+bytes([27,91,50,59"
                               ",50,72])+middle+bytes([27,91,51,59,50,72])+bottom)~%"
                               "time.sleep(0.40)~%"
                               "os.write(1,bytes([27,91,50,59,52,72,82,69,65,68,89]))~%"
                               "while True: time.sleep(1)~%"
                               )))
         (session (route-frame-capture-test-launch script 4 14))
         (first-frame-signature nil)
         (stable-through-update-p t)
         (false-checks 0)
         (predicate-completed-p nil))
    (unwind-protect
        (multiple-value-bind (octets grid metadata)
            (let ((ntop.e2e::*route-frame-stability-seconds* 0.12d0))
              (ntop.e2e::%route-capture-frame
               session (route-frame-capture-test-empty-octets) 4 14
               :side :candidate :stage :late-panel-content
               :timeout-seconds 1.5d0 :stability-seconds 0.12d0
               :completion-predicate
               (lambda (current-grid)
                 (let* ((panel-present-p
                         (and (= (char-code (ntop.e2e:cell-character
                                             (ntop.e2e:grid-cell-at current-grid 0 1)))
                                 #x250c)
                              (= (char-code (ntop.e2e:cell-character
                                             (ntop.e2e:grid-cell-at current-grid 2 1)))
                                 #x2514)
                              (= (char-code (ntop.e2e:cell-character
                                             (ntop.e2e:grid-cell-at current-grid 2 12)))
                                 #x2518)))
                        (ready-p
                         (and panel-present-p
                              (search "READY"
                                      (ntop.e2e::criterion-row-text current-grid 1)))))
                   (when panel-present-p
                     (let ((signature
                            (first
                             (multiple-value-list
                              (ntop.e2e::%route-grid-stable-signature current-grid)))))
                       (if first-frame-signature
                           (unless (equal signature first-frame-signature)
                             (setf stable-through-update-p nil))
                         (setf first-frame-signature signature))
                       (unless ready-p (incf false-checks))))
                   (when ready-p (setf predicate-completed-p t))
                   ready-p))))
          (declare (ignore octets))
          (cl-weave:expect predicate-completed-p :to-be t)
          (cl-weave:expect (>= false-checks 2) :to-be t)
          (cl-weave:expect stable-through-update-p :to-be t)
          (cl-weave:expect (eq (getf metadata :stage) :late-panel-content)
                           :to-be t)
          (cl-weave:expect
           (not (null (search "READY" (ntop.e2e::criterion-row-text grid 1))))
           :to-be t))
      (ntop.e2e::close-e2e-session session)))))

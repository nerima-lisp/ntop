(in-package #:ntop.test)

#+darwin
(cl-weave:describe
 "Darwin IOAccelerator reader"
 (cl-weave:it-run-if (member :darwin *features*)
   "calls exactly one result continuation with a contract reason"
   (let ((continuation-count 0))
     (ntop.platform.darwin:darwin-read-gpu-device-data
      (lambda (rows)
        (incf continuation-count)
        (cl-weave:expect
         (every (lambda (row)
                  (equal (loop for (key value) on row by #'cddr collect key)
                         '(:name :usage-percent :memory-used-bytes
                           :memory-total-bytes :temperature-celsius)))
                rows)
         :to-be
         t))
      (lambda (reason)
        (cl-weave:expect
         (and (keywordp reason)
              (member
               reason
               '(:not-present :disabled :permission-denied
                 :unsupported-platform :private-api-unavailable :read-failed)))
         :to-be
         t)
        (incf continuation-count)))
     (cl-weave:expect continuation-count :to-be 1))))

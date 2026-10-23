(in-package #:ntop.process.application)

(defun find-process-index-by-pid (pid rows)
  (and pid
       (position pid
                 rows
                 :key
                 #'ntop.process.domain:process-row-pid
                 :test
                 #'eql)))

(defun find-process-group-index-by-key (group-key group-keys)
  (position group-key group-keys :test #'equal))

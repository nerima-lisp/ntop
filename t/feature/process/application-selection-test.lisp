(in-package #:ntop.test)

(defun wp4-process-rows-with-pids (pids)
  (mapcar
   (lambda (pid)
     (ntop.process.domain:make-process-row :pid pid))
   pids))

(cl-weave:describe "process application selection"
                   (cl-weave:it-each
                    ((:first 10 0) (:middle 20 1)
                     (:last 30 2)
                     (:reordered 20 2)
                     (:missing 99 nil))
                    "restores normal selection by PID in the ~S case"
                    (case pid
                      expected-index)
                    (let ((processes
                           (wp4-process-rows-with-pids
                            (if (eq case :reordered)
                                '(10 30 20)
                                '(10 20 30)))))
                      (cl-weave:expect
                       (ntop.process.application::find-process-index-by-pid pid
                                                                            processes)
                       :to-equal
                       expected-index)))
                   (cl-weave:it-each
                    ((:upper-string "Worker" 0) (:lower-string "worker" 1)
                     (:missing-string "other" nil))
                    "restores grouped selection only for the exact string key in the ~S case"
                    (case key
                      expected-index)
                    (declare (ignorable case))
                    (let ((group-keys (list "Worker" "worker")))
                      (cl-weave:expect
                       (ntop.process.application::find-process-group-index-by-key
                        key
                        group-keys)
                       :to-equal
                       expected-index)))
                   (cl-weave:it
                    "returns the first index when duplicate process PIDs occur"
                    (let ((processes (wp4-process-rows-with-pids '(10 20 10))))
                      (cl-weave:expect
                       (ntop.process.application::find-process-index-by-pid 10
                                                                            processes)
                       :to-equal
                       0)))
                   (cl-weave:it "returns the first index for an equal group key"
                                (cl-weave:expect
                                 (ntop.process.application::find-process-group-index-by-key
                                  '(:name "worker")
                                  (list '(:name "worker") '(:name "worker")))
                                 :to-equal
                                 0))
                   (cl-weave:it
                    "does not restore a row when the old PID is unavailable"
                    (cl-weave:expect
                     (let ((rows (wp4-process-rows-with-pids (list nil 10))))
                       (ntop.process.application::find-process-index-by-pid nil
                                                                            rows))
                     :to-be
                     nil)))

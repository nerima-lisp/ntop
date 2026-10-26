(in-package #:ntop.e2e)

(defun %route-fixture-spawn-named-process (state name)
  (let ((process
         (sb-ext:run-program "/bin/bash"
                             (list "-c" (format nil "exec -a ~a sleep 300" name))
                             :wait
                             nil
                             :output
                             nil
                             :error
                             nil)))
    (push (list :process process :child-pids nil) (getf state :children))
    (sb-ext:process-pid process)))

(defun %route-fixture-spawn-process-tree (state)
  (let* ((process
          (sb-ext:run-program "/bin/bash"
                              (list "-c" "sleep 300 & child=$!; echo $child; wait $child")
                              :wait
                              nil
                              :output
                              :stream
                              :error
                              nil))
         (child-pid (parse-integer (read-line (sb-ext:process-output process)))))
    (close (sb-ext:process-output process))
    (push (list :process process :child-pids (list child-pid)) (getf state :children))
    (list (sb-ext:process-pid process) child-pid)))

(defun %route-fixture-start (route subcase)
  (declare (ignore route))
  (let* ((fixture (getf subcase :fixture))
         (state
          (list :fixture fixture
                :probe (getf subcase :probe)
                :after-action-input
                (and (eq (getf subcase :probe) :search-cursor) "X")
                :applicability (getf subcase :applicability)
                :children nil
                :evidence-path nil)))
    (handler-case
        (progn
          (case fixture
            ((nil :dashboard :graph :help :table :battery :capability) nil)
            (:processes
             (%route-fixture-spawn-named-process state "ntop-fixture-sleep")
             (%route-fixture-spawn-named-process state "ntop-fixture-sleepy"))
            (:process-tree (%route-fixture-spawn-process-tree state))
            (:search
             (%route-fixture-spawn-named-process state "ntop-fixture-sleep")
             (%route-fixture-spawn-named-process state "ntop-fixture-sleepy"))
            (otherwise (error "Unknown route fixture: ~s" fixture)))
          state)
      (error (condition)
        (%route-fixture-cleanup state)
        (error condition)))))

(defun %route-fixture-kill-pid (pid)
  (uiop:run-program (list "kill" "-9" (princ-to-string pid))
                    :ignore-error-status
                    t
                    :output
                    :string
                    :error-output
                    :string))

(defun %route-fixture-cleanup (state)
  (dolist (child (getf state :children))
    (dolist (pid (getf child :child-pids))
      (%route-fixture-kill-pid pid))
    (let ((process (getf child :process)))
      (when (and process (sb-ext:process-alive-p process))
        (sb-ext:process-kill process 9 :pid))
      (when process
        (sb-ext:process-wait process))))
  (setf (getf state :children) nil)
  state)

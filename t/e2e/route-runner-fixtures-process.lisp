(in-package #:ntop.e2e)

(progn
  (defun %route-fixture-process-executable (state name target)
    (let* ((directory
             (or (getf state :process-directory)
                 (setf (getf state :process-directory)
                       (sb-posix:mkdtemp
                        (copy-seq "/tmp/ntop-route-fixture.XXXXXX")))))
           (subdirectory
             (concatenate 'string directory "/"
                          (princ-to-string (length (getf state :process-subdirectories)))))
           (path (concatenate 'string subdirectory "/" name)))
      (sb-posix:mkdir subdirectory #o700)
      (push subdirectory (getf state :process-subdirectories))
      (sb-posix:symlink target path)
      (push path (getf state :process-links))
      path))

  (defun %route-fixture-query-process-names (subcase)
    (let ((prefix (getf subcase :query-fixture-prefix))
          (tags (getf subcase :query-fixture-tags)))
      (if (and (stringp prefix) (consp tags))
          (mapcar (lambda (tag) (concatenate 'string prefix tag)) tags)
          (append '("ntop-t-sleep" "ntop-t-sleepy")
                  (%route-fixture-query-extra-process-names subcase)))))

  (defun %route-fixture-spawn-named-process
      (state name &optional (target "/bin/sleep"))
    (let* ((executable (%route-fixture-process-executable state name target))
           (process
             (sb-ext:run-program executable
                                 '("300")
                                 :wait nil
                                 :output nil
                                 :error nil)))
      (push (list :process process :child-pids nil) (getf state :children))
      (sb-ext:process-pid process))))

(progn
  (defun %route-fixture-subcase-id (route subcase)
    (loop for candidate in (%route-subcases-expanded route)
          for index from 1
          when (equal candidate subcase)
            return (%route-subcase-id route index)))

  (defun %route-fixture-stage-progress (route subcase stage)
    (format *error-output*
            "~&[route-fixture] route=~a subcase=~a stage=~s~%"
            (if route (route-id route) "unknown")
            (if (and route subcase)
                (or (%route-fixture-subcase-id route subcase) "unknown")
                "unknown")
            stage)
    (force-output *error-output*))

  (defun %route-fixture-stage-error (route subcase stage detail)
    (error "Route fixture ~a subcase ~a stage ~s failed: ~a"
           (if route (route-id route) "unknown")
           (if (and route subcase)
               (or (%route-fixture-subcase-id route subcase) "unknown")
               "unknown")
           stage detail))

  (progn
    (defconstant +route-fixture-pid-line-max-characters+ 20)

    (defun %route-fixture-read-child-pid-line (stream)
      (let ((character-count 0)
            (eof-before-line-p nil)
            (too-long-p nil))
        (let ((line
                (with-output-to-string (output)
                  (loop for character = (read-char stream nil :eof)
                        do (cond
                             ((eq character :eof)
                              (setf eof-before-line-p (zerop character-count))
                              (return))
                             ((char= character #\Newline)
                              (return))
                             ((>= character-count +route-fixture-pid-line-max-characters+)
                              (setf too-long-p t)
                              (return))
                             (t
                              (write-char character output)
                              (incf character-count)))))))
          (cond
            (too-long-p (values nil t))
            (eof-before-line-p (values nil nil))
            (t (values line nil))))))

    (defun %route-fixture-read-child-pid (state process)
      (let* ((route (getf state :route))
             (subcase (getf state :subcase))
             (stream (sb-ext:process-output process)))
        (multiple-value-bind (line too-long-p)
            (handler-case
                (sb-ext:with-timeout 5
                  (%route-fixture-read-child-pid-line stream))
              (sb-ext:timeout (condition)
                (%route-fixture-stage-error
                 route subcase :process-tree-pid-read
                 (princ-to-string condition)))
              (error (condition)
                (%route-fixture-stage-error
                 route subcase :process-tree-pid-read
                 (princ-to-string condition))))
          (when too-long-p
            (%route-fixture-stage-error
             route subcase :process-tree-pid-read
             (format nil
                     "the PID line exceeded the ~d-character limit."
                     +route-fixture-pid-line-max-characters+)))
          (unless line
            (%route-fixture-stage-error
             route subcase :process-tree-pid-read "the PID stream reached EOF."))
          (unless (and (plusp (length line))
                       (every (lambda (character)
                                (and (char>= character #\0)
                                     (char<= character #\9)))
                              line))
            (%route-fixture-stage-error
             route subcase :process-tree-pid-read "the PID was not decimal digits."))
          (let ((pid (parse-integer line :junk-allowed nil)))
            (unless (plusp pid)
              (%route-fixture-stage-error
               route subcase :process-tree-pid-read "the PID was not positive."))
            pid)))))

  (defun %route-fixture-spawn-process-tree (state)
  (let* ((route (getf state :route))
         (subcase (getf state :subcase))
         (parent-name (getf subcase :fixture-tree-parent-name))
         (child-name (getf subcase :fixture-tree-child-name)))
    (%route-fixture-stage-progress route subcase :process-tree-start)
    (unless (and parent-name child-name)
      (%route-fixture-stage-error
       route subcase :process-tree-start
       "the subcase must declare tree parent and child names."))
    (let* ((parent-executable
             (%route-fixture-process-executable state parent-name "/bin/bash"))
           (child-executable
             (%route-fixture-process-executable state child-name "/bin/sleep"))
           (process
             (sb-ext:run-program
              parent-executable
              (list "-c"
                    (concatenate 'string
                                 "cleanup() { if kill -0 $child 2>/dev/null; "
                                 "then kill -KILL $child 2>/dev/null; fi; "
                                 "wait; exit 143; }; "
                                 "trap cleanup HUP INT TERM; "
                                 "read -r start || exit 1; "
                                 "exec -a $1 $2 300 & child=$!; "
                                 "echo $child; "
                                 "wait $child")
                    parent-name child-name child-executable)
              :wait nil
              :input :stream
              :output :stream
              :error nil))
           (entry (list :process process :child-pids nil)))
      (push entry (getf state :children))
      (let ((input (sb-ext:process-input process)))
        (write-line "start" input)
        (finish-output input)
        (close input))
      (%route-fixture-stage-progress route subcase :process-tree-pid-read)
      (let ((child-pid (%route-fixture-read-child-pid state process)))
        (setf (getf entry :child-pids) (list child-pid))
        (%route-fixture-stage-progress route subcase :process-tree-child-ready)
        (list (sb-ext:process-pid process) child-pid)))))
)

(defun %route-fixture-start (route subcase)
  (let* ((fixture (getf subcase :fixture))
         (state
           (list :route route
                 :subcase subcase
                 :fixture fixture
                 :probe (getf subcase :probe)
                 :after-action-input
                 (and (eq (getf subcase :probe) :search-cursor) "X")
                 :applicability (getf subcase :applicability)
                 :children nil
                 :process-directory nil
                 :process-links nil
                 :process-subdirectories nil
                 :evidence-path nil)))
    (%route-fixture-stage-progress route subcase :fixture-start)
    (handler-case
        (progn
          (case fixture
            ((nil :dashboard :graph :help :table :battery :capability) nil)
            (:processes
             (dolist (name (%route-fixture-query-process-names subcase))
               (%route-fixture-spawn-named-process state name))
             (loop for index from 1
                   to (getf subcase :fixture-process-count 0)
                   do (%route-fixture-spawn-named-process
                       state (format nil "ntop-r-~3,'0d" index))))
            (:process-tree
             (setf (getf state :tree-pids)
                   (%route-fixture-spawn-process-tree state)))
            (:search
             (dolist (name (%route-fixture-query-process-names subcase))
               (%route-fixture-spawn-named-process state name)))
            (otherwise (error "Unknown route fixture: ~s" fixture)))
          (%route-fixture-stage-progress route subcase :fixture-ready)
          state)
      (error (condition)
        (%route-fixture-stage-progress route subcase :fixture-start-error)
        (handler-case
            (%route-fixture-cleanup state)
          (error (cleanup-condition)
            (error "Route fixture startup failed: ~a; cleanup failed: ~a"
                   condition cleanup-condition)))
        (error condition)))))

(defun %route-fixture-kill-pid (pid route subcase)
  (%route-fixture-stage-progress route subcase :process-tree-child-kill)
  (handler-case
      (sb-ext:with-timeout 2
        (handler-case
            (sb-posix:kill pid sb-posix:sigkill)
          (sb-posix:syscall-error (condition)
            (unless (= (sb-posix:syscall-errno condition) sb-posix:esrch)
              (error condition)))))
    (sb-ext:timeout (condition)
      (%route-fixture-stage-error
       route subcase :process-tree-child-kill
       (format nil "timed out while signaling PID ~d: ~a"
               pid condition)))
    (error (condition)
      (%route-fixture-stage-error
       route subcase :process-tree-child-kill
       (princ-to-string condition)))))

(progn
  (defun %route-fixture-remove-process-files (state)
    (let ((failure nil))
      (labels ((remember-error (condition)
                 (unless failure (setf failure condition))))
        (dolist (path (getf state :process-links))
          (handler-case
              (when (probe-file path)
                (sb-posix:unlink path))
            (error (condition)
              (remember-error condition))))
        (dolist (subdirectory (getf state :process-subdirectories))
          (handler-case
              (when (probe-file subdirectory)
                (sb-posix:rmdir subdirectory))
            (error (condition)
              (remember-error condition))))
        (let ((directory (getf state :process-directory)))
          (when directory
            (handler-case
                (when (probe-file directory)
                  (sb-posix:rmdir directory))
              (error (condition)
                (remember-error condition)))))
        (when failure (error failure))
        (setf (getf state :process-links) nil
              (getf state :process-subdirectories) nil
              (getf state :process-directory) nil)
        state)))

  (defun %route-fixture-cleanup (state)
    (let ((route (getf state :route))
          (subcase (getf state :subcase)))
      (flet ((wait-for-process (process seconds)
               (handler-case
                   (progn
                     (sb-ext:with-timeout seconds
                       (sb-ext:process-wait process))
                     t)
                 (sb-ext:timeout () nil))))
        (let ((failure nil))
          (labels ((remember-error (condition)
                     (unless failure (setf failure condition)))
                   (cleanup-entry (child)
                     (dolist (pid (getf child :child-pids))
                       (handler-case
                           (%route-fixture-kill-pid pid route subcase)
                         (error (condition)
                           (remember-error condition))))
                     (let ((process (getf child :process)))
                       (when process
                         (when (sb-ext:process-alive-p process)
                           (%route-fixture-stage-progress
                            route subcase :process-parent-term)
                           (handler-case
                               (sb-ext:process-kill process 15 :pid)
                             (error (condition)
                               (remember-error condition))))
                         (unless (wait-for-process process 2)
                           (%route-fixture-stage-progress
                            route subcase :process-parent-kill)
                           (when (sb-ext:process-alive-p process)
                             (handler-case
                                 (sb-ext:process-kill process 9 :pid)
                               (error (condition)
                                 (remember-error condition))))
                           (unless (wait-for-process process 2)
                             (handler-case
                                 (%route-fixture-stage-error
                                  route subcase :process-parent-reap
                                  "the process remained unreaped after SIGKILL.")
                               (error (condition)
                                 (remember-error condition)))))))))
            (unwind-protect
                (progn
                  (%route-fixture-stage-progress route subcase :cleanup-start)
                  (dolist (child (getf state :children))
                    (handler-case
                        (cleanup-entry child)
                      (error (condition)
                        (remember-error condition))))
                  (setf (getf state :children) nil)
                  (%route-fixture-stage-progress route subcase :cleanup-complete)
                  (when failure (error failure))
                  state)
              (handler-case
                  (%route-fixture-remove-process-files state)
                (error (condition)
                  (if failure
                      (error "Process cleanup failed: ~a; process link cleanup failed: ~a"
                             failure condition)
                    (error condition)))))))))))

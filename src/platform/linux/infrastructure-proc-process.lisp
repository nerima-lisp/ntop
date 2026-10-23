(in-package #:ntop.platform.linux)

(defparameter +linux-process-stat-fields+
  '(:pid :comm
         :state
         :ppid
         :pgrp
         :session
         :tty-nr
         :tpgid
         :flags
         :minflt
         :cminflt
         :majflt
         :cmajflt
         :utime
         :stime
         :cutime
         :cstime
         :priority
         :nice
         :num-threads
         :itrealvalue
         :starttime
         :vsize
         :rss
         :rsslim
         :startcode
         :endcode
         :startstack
         :kstkesp
         :kstkeip
         :signal
         :blocked
         :sigignore
         :sigcatch
         :wchan
         :nswap
         :cnswap
         :exit-signal
         :processor
         :rt-priority
         :policy
         :delayacct-blkio-ticks
         :guest-time
         :cguest-time
         :start-data
         :end-data
         :start-brk
         :arg-start
         :arg-end
         :env-start
         :env-end
         :exit-code))

(defun parse-linux-process-stat-fields (pid comm state numeric-fields)
  (let ((tokens (list pid comm state))
        (numeric-values nil)
        (fields nil)
        (invalid-p nil))
    (dolist (token numeric-fields)
      (let ((value (parse-linux-integer token)))
        (if value
            (push value numeric-values)
            (setf invalid-p t))))
    (setf tokens (nconc tokens (nreverse numeric-values)))
    (when (< (length tokens) 24)
      (setf invalid-p t))
    (unless invalid-p
      (loop for field in +linux-process-stat-fields+
            for value in tokens
            do (push field fields) (push value fields))
      (setf fields (nreverse fields))
      (when (> (length tokens) (length +linux-process-stat-fields+))
        (setf fields (append fields
                             (list :extra-fields
                                   (nthcdr (length +linux-process-stat-fields+)
                                           tokens))))))
    (values fields (not invalid-p))))

(defun parse-linux-process-stat (text on-value on-unavailable)
  (let* ((line (string-trim '(#\Space #\Tab #\Return #\Newline) text))
         (open (position #\( line))
         (close (and open (search ") " line :start2 (1+ open) :from-end t)))
         (pid-fields (and open (split-linux-fields (subseq line 0 open))))
         (pid
          (and pid-fields
               (= (length pid-fields) 1)
               (parse-linux-integer (first pid-fields)))))
    (if (and pid open close)
        (let* ((comm (subseq line (1+ open) close))
               (tail (split-linux-fields (subseq line (+ close 2))))
               (state (first tail)))
          (if (and state (= (length state) 1))
              (multiple-value-bind (fields valid-p)
                  (parse-linux-process-stat-fields pid comm state (rest tail))
                (if valid-p
                    (funcall on-value fields)
                    (funcall on-unavailable :invalid-data)))
              (funcall on-unavailable :invalid-data)))
        (funcall on-unavailable :invalid-data))))

(define-linux-process-reader read-linux-process-stat
                             "stat"
                             parse-linux-process-stat)

(defun parse-linux-status-token (token)
  (or (parse-linux-integer token) token))

(defun parse-linux-process-status (text on-value on-unavailable)
  (let ((fields nil)
        (invalid-p nil))
    (dolist (line (split-linux-lines text))
      (let ((colon (position #\: line)))
        (if colon
            (let* ((key (string-trim '(#\Space #\Tab) (subseq line 0 colon)))
                   (tokens (split-linux-fields (subseq line (1+ colon))))
                   (values (mapcar #'parse-linux-status-token tokens)))
              (if (and (plusp (length key)) tokens)
                  (push
                   (cons key
                         (if (= (length values) 1)
                             (first values)
                             values))
                   fields)
                  (setf invalid-p t)))
            (unless (null (split-linux-fields line))
              (setf invalid-p t)))))
    (if (or invalid-p (null fields))
        (funcall on-unavailable :invalid-data)
        (funcall on-value (nreverse fields)))))

(define-linux-process-reader read-linux-process-status
                             "status"
                             parse-linux-process-status)

(defun parse-linux-process-io (text on-value on-unavailable)
  (let ((fields nil)
        (invalid-p nil))
    (dolist (line (split-linux-lines text))
      (let ((tokens (split-linux-fields line)))
        (when tokens
          (let ((value
                 (and (second tokens) (parse-linux-integer (second tokens)))))
            (if (and value (= (length tokens) 2))
                (push (cons (string-right-trim ":" (first tokens)) value)
                      fields)
                (setf invalid-p t))))))
    (if (or invalid-p (null fields))
        (funcall on-unavailable :invalid-data)
        (funcall on-value (nreverse fields)))))

(define-linux-process-reader read-linux-process-io "io" parse-linux-process-io)

(defun split-linux-nul-arguments (text)
  (let ((arguments nil)
        (start 0)
        (length (length text)))
    (loop for
          end = (position #\Null text :start start)
          do (if end
                 (progn
                   (push (subseq text start end) arguments)
                   (setf start (1+ end)))
                 (progn
                   (when (< start length)
                     (push (subseq text start) arguments))
                   (return)))
          while
          end)
    (nreverse arguments)))

(defun parse-linux-process-cmdline (text on-value on-unavailable)
  (declare (ignore on-unavailable))
  (funcall on-value (split-linux-nul-arguments text)))

(define-linux-process-reader read-linux-process-cmdline
                             "cmdline"
                             parse-linux-process-cmdline)

(defun read-linux-process-tasks (pid on-value on-unavailable)
  (if (and (integerp pid) (plusp pid))
      (read-linux-directory (format nil "proc/~D/task" pid)
                            (lambda (entries)
                              (let ((task-pids
                                     (loop for entry in entries
                                           for task-pid = (parse-linux-integer
                                                           entry)
                                           when (and task-pid (plusp task-pid))
                                             collect task-pid)))
                                (funcall on-value (sort task-pids (function <)))))
                            on-unavailable)
      (funcall on-unavailable :invalid-pid)))

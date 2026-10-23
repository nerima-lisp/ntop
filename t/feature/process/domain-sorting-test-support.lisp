(in-package #:ntop.test)

(defun %wp4-sorting-row (pid key value)
  (ntop.process.domain:make-process-row :count
                                        1
                                        :pid
                                        pid
                                        :name
                                        (if (eq key :name)
                                            value
                                            (format nil "name-~D" pid))
                                        :command
                                        (if (eq key :command)
                                            value
                                            (format nil "command-~D" pid))
                                        :cpu-percent
                                        (if (eq key :cpu-percent)
                                            value
                                            nil)
                                        :memory-bytes
                                        (if (eq key :memory-bytes)
                                            value
                                            nil)
                                        :memory-percent
                                        (if (eq key :memory-percent)
                                            value
                                            nil)
                                        :gpu-usage-percent
                                        (if (eq key :gpu-usage-percent)
                                            value
                                            nil)
                                        :gpu-memory-bytes
                                        (if (eq key :gpu-memory-bytes)
                                            value
                                            nil)
                                        :gpu-memory-percent
                                        (if (eq key :gpu-memory-percent)
                                            value
                                            nil)))

(defun %wp4-sorting-row-with-values (pid key value memory-mode)
  (let ((field
         (if (eq key :memory)
             (if (eq memory-mode :bytes)
                 :memory-bytes
                 :memory-percent)
             (if (eq key :gpu-memory)
                 (if (eq memory-mode :bytes)
                     :gpu-memory-bytes
                     :gpu-memory-percent)
                 key))))
    (%wp4-sorting-row pid field value)))

(defun wp4-sort-state (&rest initargs)
  (apply (function ntop.process.domain::make-process-sort-state) initargs))

(defun wp4-tree-state (&rest initargs)
  (apply (function ntop.process.domain::make-process-tree-state) initargs))

(defun %wp4-sorted-pids (rows)
  (mapcar #'ntop.process.domain:process-row-pid rows))

(defun %wp4-sorting-row-for-column (pid key value)
  (ntop.process.domain:make-process-row :count
                                        (if (eq key :count)
                                            value
                                            1)
                                        :pid
                                        pid
                                        :name
                                        (if (eq key :name)
                                            value
                                            (format nil "name-~D" pid))
                                        :command
                                        (if (eq key :command)
                                            value
                                            (format nil "command-~D" pid))
                                        :virtual-memory-bytes
                                        (if (eq key :virtual-memory-bytes)
                                            value
                                            nil)
                                        :read-bytes-per-second
                                        (if (eq key :read-bytes-per-second)
                                            value
                                            nil)
                                        :write-bytes-per-second
                                        (if (eq key :write-bytes-per-second)
                                            value
                                            nil)
                                        :total-read-bytes
                                        (if (eq key :total-read-bytes)
                                            value
                                            nil)
                                        :total-write-bytes
                                        (if (eq key :total-write-bytes)
                                            value
                                            nil)
                                        :user-name
                                        (if (eq key :user)
                                            value
                                            nil)
                                        :state
                                        (if (eq key :state)
                                            value
                                            nil)
                                        :elapsed-seconds
                                        (if (eq key :runtime-seconds)
                                            value
                                            nil)
                                        :priority
                                        (if (eq key :priority)
                                            value
                                            nil)
                                        :nice
                                        (if (eq key :nice)
                                            value
                                            nil)))

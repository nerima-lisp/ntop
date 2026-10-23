(in-package #:ntop.test)

(defun %wp4-metrics-record (pid &key
                                ppid
                                name
                                command
                                cpu-time-ns
                                resident-memory-bytes
                                virtual-memory-bytes
                                read-bytes
                                write-bytes
                                uid
                                user-name
                                state
                                start-time-unix-seconds
                                priority
                                nice
                                gpu-usage-percent
                                gpu-memory-bytes
                                ps-fallback-cpu-percent)
  (ntop.process.domain:make-process-record :pid
                                           pid
                                           :ppid
                                           ppid
                                           :name
                                           name
                                           :command
                                           command
                                           :cpu-time-ns
                                           cpu-time-ns
                                           :resident-memory-bytes
                                           resident-memory-bytes
                                           :virtual-memory-bytes
                                           virtual-memory-bytes
                                           :read-bytes
                                           read-bytes
                                           :write-bytes
                                           write-bytes
                                           :uid
                                           uid
                                           :user-name
                                           user-name
                                           :state
                                           state
                                           :start-time-unix-seconds
                                           start-time-unix-seconds
                                           :priority
                                           priority
                                           :nice
                                           nice
                                           :gpu-usage-percent
                                           gpu-usage-percent
                                           :gpu-memory-bytes
                                           gpu-memory-bytes
                                           :ps-fallback-cpu-percent
                                           ps-fallback-cpu-percent))

(defun %wp4-metrics-sample (sampled-at &optional record)
  (ntop.process.domain:make-process-sample :sampled-at
                                           sampled-at
                                           :processes
                                           (if record
                                               (list record)
                                               nil)
                                           :signals
                                           nil))

(defun %wp4-read-bytes-per-second (&rest arguments)
  (apply (function ntop.process.domain::process-read-bytes-per-second)
         arguments))

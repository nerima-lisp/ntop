(in-package #:ntop.process.domain)

(defun %sum-row-values (rows accessor)
  (let ((values (mapcar accessor rows)))
    (when (every #'identity values)
      (reduce #'+ values))))

(defun %sum-available-row-values (rows accessor)
  (let ((values (remove nil (mapcar accessor rows))))
    (when values
      (reduce #'+ values))))

(defun %maximum-row-values (rows accessor)
  (let ((values (mapcar accessor rows)))
    (when (every #'identity values)
      (reduce #'max values))))

(defun %minimum-pid-row (rows)
  (reduce
   (lambda (left right)
     (let ((left-pid (process-row-pid left))
           (right-pid (process-row-pid right)))
       (cond ((null left-pid) right)
             ((null right-pid) left)
             ((< right-pid left-pid) right)
             (t left))))
   rows))

(defun %row-count (row)
  (or (process-row-count row) 1))

(defun %aggregate-process-rows (rows representative)
  (make-process-row
   :count (reduce #'+ rows :key #'%row-count)
   :pid (process-row-pid representative)
   :ppid (process-row-ppid representative)
   :name (process-row-name representative)
   :command (process-row-command representative)
   :cpu-percent (%sum-row-values rows #'process-row-cpu-percent)
   :memory-percent (%sum-available-row-values rows #'process-row-memory-percent)
   :memory-bytes (%sum-available-row-values rows #'process-row-memory-bytes)
   :virtual-memory-bytes (process-row-virtual-memory-bytes representative)
   :read-bytes-per-second
   (%sum-row-values rows #'process-row-read-bytes-per-second)
   :write-bytes-per-second
   (%sum-row-values rows #'process-row-write-bytes-per-second)
   :total-read-bytes (%sum-row-values rows #'process-row-total-read-bytes)
   :total-write-bytes (%sum-row-values rows #'process-row-total-write-bytes)
   :user-name (process-row-user-name representative)
   :state (process-row-state representative)
   :elapsed-seconds (%maximum-row-values rows #'process-row-elapsed-seconds)
   :priority (process-row-priority representative)
   :nice (process-row-nice representative)
   :gpu-usage-percent (%sum-row-values rows #'process-row-gpu-usage-percent)
   :gpu-memory-bytes (%sum-row-values rows #'process-row-gpu-memory-bytes)
   :gpu-memory-percent (%sum-row-values rows #'process-row-gpu-memory-percent)))

(defun %process-grouping-value (row key-kind)
  (ecase key-kind
    (:name (process-row-name row))
    (:command (process-row-command row))))

(defun group-process-domain-rows (rows key-kind)
  (let ((groups-by-key (make-hash-table :test #'equal))
        (groups (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (row rows)
      (let ((group-key (list key-kind (%process-grouping-value row key-kind))))
        (multiple-value-bind (index present-p)
            (gethash group-key groups-by-key)
          (if present-p
              (let* ((previous (aref groups index))
                     (members (list previous row))
                     (representative (%minimum-pid-row members)))
                (setf (aref groups index)
                      (%aggregate-process-rows members representative)))
              (let ((index (length groups)))
                (setf (gethash group-key groups-by-key) index)
                (vector-push-extend
                 (%aggregate-process-rows (list row) row)
                 groups))))))
    (coerce groups 'list)))

(in-package #:ntop.process.domain)

(defparameter +process-sort-default-orders+
  '((:cpu-percent . :descending)
    (:memory . :descending)
    (:virtual-memory-bytes . :descending)
    (:pid . :ascending)
    (:count . :ascending)
    (:name . :ascending)
    (:command . :ascending)
    (:read-bytes-per-second . :descending)
    (:write-bytes-per-second . :descending)
    (:total-read-bytes . :descending)
    (:total-write-bytes . :descending)
    (:state . :ascending)
    (:user . :ascending)
    (:runtime-seconds . :ascending)
    (:priority . :descending)
    (:nice . :ascending)
    (:gpu-usage-percent . :descending)
    (:gpu-memory . :descending)))

(defparameter +process-sort-character-keys+
  '((#\c . :cpu-percent)
    (#\m . :memory)
    (#\p . :pid)
    (#\n . :name)
    (#\C . :gpu-usage-percent)
    (#\M . :gpu-memory)))

(defstruct (process-sort-state
            (:constructor %make-process-sort-state (key order memory-mode)))
  key
  order
  memory-mode)

(defun %process-sort-default-order (key)
  (or (cdr (assoc key +process-sort-default-orders+))
      (error "Unsupported process sort key: ~S" key)))

(defun make-process-sort-state
    (&key (key :cpu-percent) order (memory-mode :percent))
  (unless (member memory-mode '(:bytes :percent))
    (error "Unsupported process memory sort mode: ~S" memory-mode))
  (%make-process-sort-state
   key
   (or order (%process-sort-default-order key))
   memory-mode))

(defun process-sort-key-from-character (character &key command-p)
  (let ((key (cdr (assoc character +process-sort-character-keys+))))
    (when key
      (if (and (char= character #\n) command-p)
          :command
          key))))

(defun process-sort-select-key (state key)
  (%make-process-sort-state
   key
   (if (eq key (process-sort-state-key state))
       (if (eq (process-sort-state-order state) :ascending)
           :descending
           :ascending)
       (%process-sort-default-order key))
   (process-sort-state-memory-mode state)))

(defun process-sort-invert (state)
  (%make-process-sort-state
   (process-sort-state-key state)
   (if (eq (process-sort-state-order state) :ascending)
       :descending
       :ascending)
   (process-sort-state-memory-mode state)))

(defun %process-row-sort-value (row state)
  (ecase (process-sort-state-key state)
    (:cpu-percent (process-row-cpu-percent row))
    (:memory
     (ecase (process-sort-state-memory-mode state)
       (:bytes (process-row-memory-bytes row))
       (:percent (process-row-memory-percent row))))
    (:virtual-memory-bytes (process-row-virtual-memory-bytes row))
    (:pid (process-row-pid row))
    (:count (process-row-count row))
    (:name (process-row-name row))
    (:command (process-row-command row))
    (:read-bytes-per-second (process-row-read-bytes-per-second row))
    (:write-bytes-per-second (process-row-write-bytes-per-second row))
    (:total-read-bytes (process-row-total-read-bytes row))
    (:total-write-bytes (process-row-total-write-bytes row))
    (:state (process-row-state row))
    (:user (process-row-user-name row))
    (:runtime-seconds (process-row-elapsed-seconds row))
    (:priority (process-row-priority row))
    (:nice (process-row-nice row))
    (:gpu-usage-percent (process-row-gpu-usage-percent row))
    (:gpu-memory
     (ecase (process-sort-state-memory-mode state)
       (:bytes (process-row-gpu-memory-bytes row))
       (:percent (process-row-gpu-memory-percent row))))))

(defun %compare-process-sort-values (left right case-insensitive-p)
  (cond ((equal left right) 0)
        ((null left) -1)
        ((null right) 1)
        ((and case-insensitive-p (stringp left) (stringp right))
         (let ((left-key (string-downcase left))
               (right-key (string-downcase right)))
           (cond ((string< left-key right-key) -1)
                 ((string> left-key right-key) 1)
                 (t 0))))
        ((and (stringp left) (stringp right))
         (cond ((string< left right) -1)
               ((string> left right) 1)
               (t 0)))
        ((< left right) -1)
        ((> left right) 1)
        (t 0)))

(defun %process-row-less-p (left right state)
  (let ((comparison
          (%compare-process-sort-values
           (%process-row-sort-value left state)
           (%process-row-sort-value right state)
           (member (process-sort-state-key state) '(:name :command)))))
    (if (eq (process-sort-state-order state) :ascending)
        (minusp comparison)
        (plusp comparison))))

(defun sort-process-domain-rows (rows state)
  (stable-sort (copy-list rows)
               (lambda (left right)
                 (%process-row-less-p left right state))))

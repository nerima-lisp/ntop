(in-package #:ntop.disk.domain)

(defparameter +disk-sort-keys+
  '(:disk :mount
          :filesystem-type
          :used
          :free
          :total
          :used-percent
          :free-percent
          :read-rate
          :write-rate
          :io-key))

(defun %compare-disk-values (left right)
  (cond
    ((and (null left) (null right)) 0)
    ((null left) 1)
    ((null right) -1)
    ((and (stringp left) (stringp right))
     (cond
       ((string< left right) -1)
       ((string> left right) 1)
       (t 0)))
    ((and (realp left) (realp right))
     (cond
       ((< left right) -1)
       ((> left right) 1)
       (t 0)))
    (t
     (error "Disk sort values must both be strings or real numbers: ~S, ~S"
            left
            right))))

(defun %disk-row-tie-less-p (left right)
  (dolist (key '(:disk :mount) nil)
    (let ((comparison (%compare-disk-values (getf left key) (getf right key))))
      (unless (zerop comparison)
        (return-from %disk-row-tie-less-p
          (minusp comparison))))))

(defun %disk-row-less-p (left right sort-key sort-order)
  (let ((left-value (getf left sort-key))
        (right-value (getf right sort-key)))
    (cond
      ((and (null left-value) (not (null right-value))) nil)
      ((and (not (null left-value)) (null right-value)) t)
      (t
       (let ((comparison (%compare-disk-values left-value right-value)))
         (if (zerop comparison)
             (%disk-row-tie-less-p left right)
             (if (eq sort-order :ascending)
                 (minusp comparison)
                 (plusp comparison))))))))

(defun sort-disk-rows (rows &key (sort-key :disk) (sort-order :ascending))
  (unless (member sort-key +disk-sort-keys+)
    (error "Unsupported disk sort key: ~S" sort-key))
  (unless (member sort-order '(:ascending :descending))
    (error "Unsupported disk sort order: ~S" sort-order))
  (stable-sort (copy-list rows)
               (lambda (left right)
                 (%disk-row-less-p left right sort-key sort-order))))

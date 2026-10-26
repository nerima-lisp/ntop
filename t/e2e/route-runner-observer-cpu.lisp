(in-package #:ntop.e2e)

(defun %route-observer-cpu-mhz-values (grid)
  (let* ((frame
          (and grid
               (find-if
                (lambda (candidate)
                  (search "CPU"
                          (criterion-row-text grid (first candidate))
                          :test
                          (function char-equal)))
                (criterion-projection-frames grid))))
         (header-row
          (and frame
               (loop for row from (1+ (first frame)) below (second frame)
                     for line = (criterion-row-text grid row)
                     when (and (criterion-label-spans line "MHz")
                               (criterion-label-spans line "CPU"))
                       return row)))
         (span
          (and header-row
               (first (criterion-label-spans (criterion-row-text grid header-row) "MHz")))))
    (when (and frame span)
      (loop for row from (1+ header-row) below (second frame)
            for line = (criterion-row-text grid row)
            for start = (max (1+ (third frame)) (1- (car span)))
            for
            end = (min (fourth frame) (+ start 4))
            for value = (parse-integer line :start start :end end :junk-allowed t)
            when (and value (plusp value))
              collect value))))

(defun %route-observer-average-cluster-values (before after)
  (let ((names
         (remove-duplicates (append (mapcar #'car before) (mapcar #'car after)) :test #'equal)))
    (loop for name in names
          for left = (assoc name before :test #'equal)
          for right = (assoc name after :test #'equal)
          when (and left
                    right
                    (numberp (cdr left))
                    (numberp (cdr right))
                    (plusp (cdr left))
                    (plusp (cdr right)))
            collect (cons name (/ (+ (cdr left) (cdr right)) 2.0d0)))))

(defun %route-observer-frequency-match-p (observed expected)
  (and (numberp observed)
       (numberp expected)
       (plusp expected)
       (<= (/ (abs (- observed expected)) expected) 0.1d0)))

(defun %route-observer-cpu-frequency-p (side grid)
  (let* ((displayed (%route-observer-cpu-mhz-values grid))
         (before (getf side :powermetrics-before-values))
         (after (getf side :powermetrics-after-values))
         (clusters (%route-observer-average-cluster-values before after))
         (matches-p
          (and displayed
               clusters
               (every
                (lambda (value)
                  (= 1
                     (count-if
                      (lambda (cluster)
                        (%route-observer-frequency-match-p value (cdr cluster)))
                      clusters)))
                displayed)
               (every
                (lambda (cluster)
                  (some
                   (lambda (value)
                     (%route-observer-frequency-match-p value (cdr cluster)))
                   displayed))
                clusters))))
    (values matches-p
            (list :ui-mhz displayed :cluster-averages clusters)
            (unless matches-p
              "CPU MHz values did not map uniquely to powermetrics clusters within 10%"))))

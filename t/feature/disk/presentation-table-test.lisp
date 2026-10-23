(in-package #:ntop.test)

(defstruct (%disk-screen-target (:constructor %make-disk-screen-target (cells)))
  cells)

(defmethod ntop.ui:draw-table-text ((target %disk-screen-target) x y text role)
  (declare (ignore role))
  (loop for character across text
        for column from x
        when (and (<= 0 y)
                  (< y (array-dimension (%disk-screen-target-cells target) 0))
                  (<= 0 column)
                  (< column
                     (array-dimension (%disk-screen-target-cells target) 1)))
          do (setf (aref (%disk-screen-target-cells target) y column) character)))

(defun %disk-screen-target (width height)
  (%make-disk-screen-target
   (make-array (list height width) :initial-element #\Space)))

(defun %disk-screen-lines (target)
  (loop for row below (array-dimension (%disk-screen-target-cells target) 0)
        collect (string-right-trim '(#\Space)
                                   (coerce
                                    (loop for column below (array-dimension
                                                            (%disk-screen-target-cells
                                                             target)
                                                            1)
                                          collect (aref
                                                   (%disk-screen-target-cells
                                                    target)
                                                   row
                                                   column))
                                    'string))))

(defun %disk-screen-golden (target)
  (format nil "~{~A~%~}" (%disk-screen-lines target)))

(defun %disk-table-fixture-rows ()
  '((:disk "disk4"
           :mount
           "/Volumes/Data"
           :filesystem-type
           "apfs"
           :used
           5368709120
           :free
           4294967296
           :total
           9663676416
           :used-percent
           55.6
           :free-percent
           44.4
           :read-rate
           12345
           :write-rate
           1234567890
           :io-key
           "disk4")
    (:disk "disk1"
           :mount
           "/"
           :filesystem-type
           "apfs"
           :used
           100000000000
           :free
           200000000000
           :total
           300000000000
           :used-percent
           33.3
           :free-percent
           66.7
           :read-rate
           0
           :write-rate
           0
           :io-key
           "disk1")))

(defun %render-disk-table-fixture ()
  (let* ((target (%disk-screen-target 96 4))
         (config (ntop.disk.presentation:make-disk-table-config))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 96 :height 4)))
    (ntop.ui:draw-widget :disk
                         target
                         rectangle
                         (ntop.ui:make-table-state)
                         config
                         (%disk-table-fixture-rows))
    (%disk-screen-golden target)))

(defun %disk-column-ids (columns)
  (mapcar #'ntop.ui:table-column-id columns))

(cl-weave:describe "Disk table presentation"
                   (cl-weave:it "defines the btm default columns and widths"
                                (cl-weave:expect
                                 (%disk-column-ids
                                  ntop.disk.presentation:*disk-table-columns*)
                                 :to-equal
                                 '(:disk :mount
                                         :used
                                         :free
                                         :total
                                         :used-percent
                                         :read-rate
                                         :write-rate))
                                (cl-weave:expect
                                 (mapcar #'ntop.ui:table-column-width
                                         (nthcdr 2
                                                 ntop.disk.presentation:*disk-table-columns*))
                                 :to-equal
                                 '(8 8 9 9 10 11))
                                (cl-weave:expect
                                 (mapcar #'ntop.ui:table-column-max-percent
                                         (subseq
                                          ntop.disk.presentation:*disk-table-columns*
                                          0
                                          2))
                                 :to-equal
                                 '(1/5 1/5)))
                   (cl-weave:it "makes Free% available as an optional column"
                                (cl-weave:expect
                                 (%disk-column-ids
                                  ntop.disk.presentation:*disk-table-columns-with-free-percent*)
                                 :to-equal
                                 '(:disk :mount
                                         :used
                                         :free
                                         :total
                                         :used-percent
                                         :free-percent
                                         :read-rate
                                         :write-rate))
                                (cl-weave:expect
                                 (ntop.ui:table-config-columns
                                  (ntop.disk.presentation:make-disk-table-config
                                   :show-free-percent-p
                                   t))
                                 :to-be
                                 ntop.disk.presentation:*disk-table-columns-with-free-percent*))
                   (cl-weave:it "defaults to Disk ascending"
                                (let ((config
                                       (ntop.disk.presentation:make-disk-table-config)))
                                  (cl-weave:expect
                                   (ntop.ui:table-config-sort-column config)
                                   :to-be
                                   :disk)
                                  (cl-weave:expect
                                   (ntop.ui:table-config-sort-order config)
                                   :to-be
                                   :ascending)))
                   (cl-weave:it "sorts rendered rows by the configured column"
                                (let* ((target (%disk-screen-target 96 4))
                                       (config
                                        (ntop.disk.presentation:make-disk-table-config))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                96
                                                                :height
                                                                4)))
                                  (ntop.ui:draw-widget :disk
                                                       target
                                                       rectangle
                                                       (ntop.ui:make-table-state)
                                                       config
                                                       (%disk-table-fixture-rows))
                                  (cl-weave:expect
                                   (mapcar
                                    (lambda (line)
                                      (subseq line 0 5))
                                    (subseq (%disk-screen-lines target) 2))
                                   :to-equal
                                   '("disk1" "disk4"))))
                   (cl-weave:it "matches the fixture-data screen golden"
                                (let* ((golden-path
                                        (asdf:system-relative-pathname "ntop"
                                                                       "t/feature/disk/goldens/presentation-table-96x4.txt"))
                                       (expected
                                        (string-right-trim '(#\Newline #\Return)
                                                           (uiop:read-file-string
                                                            golden-path))))
                                  (cl-weave:expect (%render-disk-table-fixture)
                                                   :to-equal
                                                   (format nil "~A~%" expected))))
                   (cl-weave:it
                    "evaluates every column formatter for present and missing values"
                    (let ((columns
                           ntop.disk.presentation:*disk-table-columns-with-free-percent*)
                          (rows (%disk-table-fixture-rows)))
                      (dolist (row rows)
                        (cl-weave:expect
                         (mapcar
                          (lambda (column)
                            (funcall
                             (ntop.ui:table-column-value-function column)
                             row))
                          columns)
                         :to-equal
                         (list
                          (ntop.disk.presentation::%disk-row-text row :disk)
                          (ntop.disk.presentation::%disk-row-text row :mount)
                          (ntop.disk.presentation::%disk-row-capacity row :used)
                          (ntop.disk.presentation::%disk-row-capacity row :free)
                          (ntop.disk.presentation::%disk-row-capacity row
                                                                      :total)
                          (ntop.disk.presentation::%disk-row-percent row
                                                                     :used-percent)
                          (ntop.disk.presentation::%disk-row-percent row
                                                                     :free-percent)
                          (ntop.disk.presentation::%disk-row-rate row
                                                                  :read-rate)
                          (ntop.disk.presentation::%disk-row-rate row
                                                                  :write-rate))))
                      (cl-weave:expect
                       (mapcar
                        (lambda (column)
                          (funcall (ntop.ui:table-column-value-function column)
                                   nil))
                        columns)
                       :to-equal
                       (make-list (length columns) :initial-element "N/A")))))

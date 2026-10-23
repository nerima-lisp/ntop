(in-package #:ntop.test)

(defstruct (%f8b-process-table-target (:constructor %f8b-make-process-table-target (cells roles)))
  cells
  roles)

(defmethod ntop.ui:draw-table-text ((target %f8b-process-table-target) x y text role)
  (loop for character across text
        for column from x
        when (and (<= 0 y)
                  (< y (array-dimension (%f8b-process-table-target-cells target) 0))
                  (<= 0 column)
                  (< column (array-dimension (%f8b-process-table-target-cells target) 1)))
          do (setf (aref (%f8b-process-table-target-cells target) y column) character
                   (aref (%f8b-process-table-target-roles target) y column) role)))

(defun %f8b-process-table-target (width height)
  (%f8b-make-process-table-target (make-array (list height width) :initial-element #\Space)
                                  (make-array (list height width) :initial-element nil)))

(defun %f8b-process-table-lines (target)
  (loop for row below (array-dimension (%f8b-process-table-target-cells target) 0)
        collect (string-right-trim '(#\Space #\Return)
                                   (coerce
                                    (loop for column below (array-dimension
                                                            (%f8b-process-table-target-cells target)
                                                            1)
                                          collect (aref (%f8b-process-table-target-cells target)
                                                        row
                                                        column))
                                    'string))))

(defun %f8b-process-table-row (pid name cpu memory-percent memory-bytes elapsed-seconds)
  (ntop.process.domain:make-process-row :count
                                        1
                                        :pid
                                        pid
                                        :name
                                        name
                                        :command
                                        (format nil "~A --demo" name)
                                        :cpu-percent
                                        cpu
                                        :memory-percent
                                        memory-percent
                                        :memory-bytes
                                        memory-bytes
                                        :virtual-memory-bytes
                                        (* memory-bytes 4)
                                        :read-bytes-per-second
                                        pid
                                        :write-bytes-per-second
                                        (* pid 2)
                                        :total-read-bytes
                                        (* pid 100)
                                        :total-write-bytes
                                        (* pid 200)
                                        :user-name
                                        "sample-user"
                                        :state
                                        "sleeping"
                                        :elapsed-seconds
                                        elapsed-seconds
                                        :priority
                                        20
                                        :gpu-usage-percent
                                        3d0
                                        :gpu-memory-bytes
                                        2048
                                        :gpu-memory-percent
                                        4d0))

(defparameter +f8b-rows+
  (loop for row-data in '((101 "worker" 12.5d0 10.0d0 1024 60d0) (102 "shell" 3.0d0 5.0d0 2048 3d0)
                          (103 "runner" 8.0d0 15.0d0 3072 42d0)
                          (104 "monitor" 2.0d0 7.0d0 4096 3600d0)
                          (105 "service" 1.0d0 2.0d0 5120 86400d0))
        collect (apply #'%f8b-process-table-row row-data)))

(defparameter +f8b-table-golden-path+
  (asdf:system-relative-pathname "ntop/test"
                                 "t/feature/process/goldens/presentation-table-96x4.txt"))

(defun %f8b-process-table-entry (name depth last-child-p has-children-p collapsed-p)
  (ntop.process.domain::%make-process-tree-entry (%f8b-process-table-row 111 name 1d0 1d0 1024 1d0)
                                                 depth
                                                 last-child-p
                                                 has-children-p
                                                 collapsed-p))

(defun %f8b-process-table-column (config &optional id)
  (let ((columns (ntop.process.presentation::process-table-visible-columns config)))
    (if id
        (find id columns :key (function ntop.ui:table-column-id))
        columns)))

(defun %f8b-display-rows (config rows)
  (ntop.process.presentation::%process-table-display-rows config rows))

(defun %f8b-config (&rest args)
  (apply #'ntop.process.presentation::make-process-table-config args))

(defun %f8b-sort-state (key order)
  (ntop.process.domain:make-process-sort-state :key key :order order))

(defun %f8b-draw-process-table (target state config rows)
  (let ((rect
         (ntop.ui:make-rectangle :x
                                 0
                                 :y
                                 0
                                 :width
                                 (array-dimension (%f8b-process-table-target-cells target) 1)
                                 :height
                                 (array-dimension (%f8b-process-table-target-cells target) 0))))
    (ntop.ui:draw-widget :process-table target rect state config rows)))

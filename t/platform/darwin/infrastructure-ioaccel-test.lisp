(in-package #:ntop.test)

(defun %ioaccel-cf-dictionary-tree (entries)
  (list :dictionary
        (mapcar (lambda (entry)
                  (cons (list :string (car entry)) (cdr entry)))
                entries)))

(cl-weave:describe
 "IOAccelerator property fixtures"
 (cl-weave:it "converts PerformanceStatistics values to a GPU row"
   (let* ((tree
            (%ioaccel-cf-dictionary-tree
             `(("IOClass" . (:string "AGXAcceleratorG16X"))
               ("PerformanceStatistics"
                . ,(%ioaccel-cf-dictionary-tree
                    '(("Device Utilization %" . (:number 33))
                      ("In use system memory" . (:number 2216853504))
                      ("Alloc system memory" . (:number 3191881728))))))))
          (properties (ntop.platform.darwin::cf-tree-to-lisp tree)))
     (cl-weave:expect
      (ntop.platform.darwin::decode-darwin-ioaccel-device-properties
       properties)
      :to-equal
      '(:name "AGXAcceleratorG16X"
        :usage-percent 33
        :memory-used-bytes 2216853504
        :memory-total-bytes 3191881728
        :temperature-celsius nil))))
 (cl-weave:it "preserves a temperature when the statistics key exists"
   (let* ((tree
            (%ioaccel-cf-dictionary-tree
             `(("IOClass" . (:string "FixtureAccelerator"))
               ("PerformanceStatistics"
                . ,(%ioaccel-cf-dictionary-tree
                    '(("Device Utilization %" . (:number 0.0d0))
                      ("In use system memory" . (:number 0))
                      ("Alloc system memory" . (:number 0))
                      ("Temperature" . (:number 47.5d0))))))))
          (properties (ntop.platform.darwin::cf-tree-to-lisp tree)))
     (cl-weave:expect
      (ntop.platform.darwin::decode-darwin-ioaccel-device-properties
       properties)
      :to-equal
      '(:name "FixtureAccelerator"
        :usage-percent 0.0d0
        :memory-used-bytes 0
        :memory-total-bytes 0
        :temperature-celsius 47.5d0))))
 (cl-weave:it "rejects a non-dictionary PerformanceStatistics value"
   (let ((properties (make-hash-table :test #'equal)))
     (setf (gethash "PerformanceStatistics" properties) nil)
     (cl-weave:expect
      (lambda ()
        (ntop.platform.darwin::decode-darwin-ioaccel-device-properties
         properties))
      :to-throw
      'error)))
)
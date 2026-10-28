(in-package #:ntop.e2e)
(defparameter *condition-a-categories*
  (quote (("border" :border-cells :border-sgr)
          ("title" :title-cells :title-sgr)
          ("process-header" nil :process-header-sgr))))
(defun %condition-a-category-entries (projection category)
  (loop for entry across (aref projection 2)
        when (string= (aref entry 0) category)
        collect entry))
(defun %condition-a-process-header-ready-p (process-frame projection)
  (and process-frame
       (or (<= (- (second process-frame) (first process-frame)) 2)
           (plusp
            (length
             (%condition-a-category-entries projection "process-header"))))))
(defun %condition-a-entry-shapes (entries)
  (mapcar (lambda (entry) (subseq entry 0 4)) entries))
(defun %condition-a-shared-style-difference-p (reference candidate)
  (loop for reference-entry in reference
        for candidate-entry =
        (find-if (lambda (entry)
                   (and (= (aref entry 1) (aref reference-entry 1))
                        (= (aref entry 2) (aref reference-entry 2))))
                 candidate)
        thereis (and candidate-entry
                     (not (equalp (subseq reference-entry 4 7)
                                  (subseq candidate-entry 4 7))))))
(defun condition-a-difference-kinds (reference-grid candidate-grid)
  (let* ((reference-frames (criterion-projection-frames reference-grid))
         (candidate-frames (criterion-projection-frames candidate-grid))
         (reference (project-criterion-grid reference-grid))
         (candidate (project-criterion-grid candidate-grid))
         (differences nil))
    (unless (equalp reference-frames candidate-frames)
      (push :frame-geometry differences))
    (dolist (category *condition-a-categories*)
      (destructuring-bind (name cells-kind style-kind) category
        (let ((reference-entries
               (%condition-a-category-entries reference name))
              (candidate-entries
               (%condition-a-category-entries candidate name)))
          (when (and cells-kind
                     (not (equalp (%condition-a-entry-shapes reference-entries)
                                  (%condition-a-entry-shapes candidate-entries))))
            (push cells-kind differences))
          (when (%condition-a-shared-style-difference-p
                 reference-entries candidate-entries)
            (push style-kind differences)))))
    (nreverse differences)))

(defun %condition-a-first-difference
    (reference-grid candidate-grid difference-kinds)
  (let ((*print-pretty* nil))
    (let ((reference-frames (criterion-projection-frames reference-grid))
          (candidate-frames (criterion-projection-frames candidate-grid)))
      (when (member :frame-geometry difference-kinds)
        (return-from %condition-a-first-difference
          (format nil "frame-geometry reference=~s candidate=~s"
                  reference-frames candidate-frames))))
    (let ((reference (project-criterion-grid reference-grid))
          (candidate (project-criterion-grid candidate-grid)))
      (dolist (category *condition-a-categories*)
        (destructuring-bind (name cells-kind style-kind) category
          (when (or (member cells-kind difference-kinds)
                    (member style-kind difference-kinds))
            (let ((left (%condition-a-category-entries reference name))
                  (right (%condition-a-category-entries candidate name)))
              (if cells-kind
                  (loop for index below (max (length left) (length right))
                        for left-entry = (nth index left)
                        for right-entry = (nth index right)
                        unless (equalp left-entry right-entry)
                          do (return-from %condition-a-first-difference
                               (format nil "~a reference=~s candidate=~s"
                                       name left-entry right-entry)))
                (return-from %condition-a-first-difference
                  (format nil "~a SGR attributes differ" name)))))))
      (if difference-kinds
          (string-downcase (symbol-name (first difference-kinds)))
          "none"))))
(defun %condition-a-capture
    (program arguments rows columns timeout-seconds environment raw-path)
  (let* ((session (launch-e2e-process program arguments rows columns
                                      :environment environment
                                      :timeout-seconds timeout-seconds))
         (raw (make-array 0 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0)))
    (unwind-protect
        (multiple-value-bind (updated grid metadata)
            (%route-capture-frame
             session raw rows columns
             :side *route-frame-capture-side*
             :stage :condition-a
             :timeout-seconds timeout-seconds
             :stability-seconds *route-frame-stability-seconds*
             :completion-predicate
             (lambda (stable-grid)
               (let* ((projection (project-criterion-grid stable-grid))
                      (process-frame
                        (find-if
                         (lambda (frame)
                           (search "Processes"
                                   (criterion-row-text stable-grid (first frame))
                                   :test #'char-equal))
                         (criterion-projection-frames stable-grid))))
                 (and (plusp
                       (length (criterion-projection-frames stable-grid)))
                      (%condition-a-process-header-ready-p
                       process-frame projection)))))
          (setf raw updated)
          (unless (sb-ext:process-alive-p (e2e-session-process session))
            (error "~a exited before its terminal frame stabilized."
                   program))
          (values grid metadata))
      (let ((captured (e2e-session-output session)))
        (when captured (setf raw captured)))
      (unwind-protect
          (when (plusp (length raw))
            (%write-octets raw-path raw))
        (close-e2e-session session)))))



(defun %condition-a-raw-path (directory program width height)
  (merge-pathnames
   (format nil "~a-~d-~d.ansi"
           (if (string= (pathname-name (pathname program)) "btm")
               "btm" "ntop")
           width height)
   directory))
(defun run-condition-a-comparison
    (&key (btm "btm") (ntop "ntop")
          (output "/tmp/ntop-fr001-condition-a.tsv")
          (rate 700) (timeout-seconds 90d0))
  (unless (and (integerp rate) (plusp rate))
    (error "RATE must be a positive integer."))
  (let* ((temporary-root (%make-e2e-temporary-directory))
         (config-path (merge-pathnames "btm-config.toml" temporary-root))
         (output-path (pathname output))
         (raw-directory
          (uiop:ensure-directory-pathname
           (merge-pathnames
            (make-pathname :name
                           (format nil "~a-raw" (pathname-name output-path))
                           :type nil)
            (uiop:pathname-directory-pathname output-path))))
         (environment (%route-environment))
         (dimensions (golden-dimensions))
         (mismatch-count 0))
    (ensure-directories-exist (merge-pathnames ".raw" raw-directory))
    (%write-text config-path (btm-config-string rate))
    (with-open-file (stream output-path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (loop for column in '("width" "height" "frame_geometry" "border_cells"
                            "border_sgr" "title_cells" "title_sgr"
                            "process_header_sgr" "differences" "first_difference")
            for first-p = t then nil
            do (unless first-p (write-char #\Tab stream))
               (write-string column stream))
      (terpri stream)
      (loop for (width height) in dimensions
            for reference-path =
              (%condition-a-raw-path raw-directory btm width height)
            for candidate-path =
              (%condition-a-raw-path raw-directory ntop width height)
            for reference-grid =
              (%condition-a-capture btm (list "-C" (namestring config-path))
                                    height width timeout-seconds
                                    environment reference-path)
            for candidate-grid =
              (%condition-a-capture ntop (list "--rate"
                                               (format nil "~dms" rate))
                                    height width timeout-seconds
                                    environment candidate-path)
            for difference-kinds =
              (condition-a-difference-kinds reference-grid candidate-grid)
            for first-difference =
              (%condition-a-first-difference reference-grid candidate-grid
                                             difference-kinds)
            do (when difference-kinds (incf mismatch-count))
               (format stream "~d" width)
               (write-char #\Tab stream)
               (format stream "~d" height)
               (dolist (kind '(:frame-geometry :border-cells :border-sgr
                               :title-cells :title-sgr :process-header-sgr))
                 (write-char #\Tab stream)
                 (write-string (if (member kind difference-kinds)
                                   "false" "true")
                               stream))
               (write-char #\Tab stream)
               (write-string (if difference-kinds
                                 (format nil "~{~(~a~)~^,~}" difference-kinds)
                               "match")
                             stream)
               (write-char #\Tab stream)
               (write-string first-difference stream)
               (terpri stream)
               (force-output stream)))
    (format t "Compared ~d dimensions; ~d have differences. Table: ~a; raw ANSI: ~a~%"
            (length dimensions) mismatch-count (namestring output-path)
            (namestring raw-directory))
    (if (zerop mismatch-count) 0 1)))

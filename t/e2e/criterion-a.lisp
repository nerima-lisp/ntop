(in-package #:ntop.e2e)
(progn
  (defparameter *condition-a-observation-seconds* 0.3d0)
  (defparameter *condition-a-first-frame-factor* 3d0)
  (defun %condition-a-category-entries (projection category)
    (loop for entry across (aref projection 2)
          when (string= (aref entry 0) category)
          collect entry))
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
                                    (subseq candidate-entry 4 7)))))
    )
  (defun condition-a-difference-kinds (reference-grid candidate-grid)
    (let* ((reference-frames (criterion-projection-frames reference-grid))
           (candidate-frames (criterion-projection-frames candidate-grid))
           (reference (project-criterion-grid reference-grid))
           (candidate (project-criterion-grid candidate-grid))
           (differences nil))
      (unless (equalp reference-frames candidate-frames)
        (push :frame-geometry differences))
      (dolist (category
               '(("border" :border-cells :border-sgr)
                 ("title" :title-cells :title-sgr)
                 ("process-header" :process-header-cells
                                   :process-header-sgr)))
        (destructuring-bind (name cells-kind style-kind) category
          (let ((reference-entries
                 (%condition-a-category-entries reference name))
                (candidate-entries
                 (%condition-a-category-entries candidate name)))
            (unless (equalp (%condition-a-entry-shapes reference-entries)
                            (%condition-a-entry-shapes candidate-entries))
              (push cells-kind differences))
            (when (%condition-a-shared-style-difference-p
                   reference-entries candidate-entries)
              (push style-kind differences)))))
      (nreverse differences)))

  (defun %condition-a-first-difference
      (reference-grid candidate-grid difference-kinds)
    (let ((reference-frames (criterion-projection-frames reference-grid))
          (candidate-frames (criterion-projection-frames candidate-grid)))
      (when (member :frame-geometry difference-kinds)
        (return-from %condition-a-first-difference
                     (format nil "frame-geometry reference=~s candidate=~s"
                             reference-frames candidate-frames))))
    (let ((reference (project-criterion-grid reference-grid))
          (candidate (project-criterion-grid candidate-grid)))
      (dolist (category
               '(("border" :border-cells :border-sgr)
                 ("title" :title-cells :title-sgr)
                 ("process-header" :process-header-cells
                                   :process-header-sgr)))
        (destructuring-bind (name cells-kind style-kind) category
          (when (or (member cells-kind difference-kinds)
                    (member style-kind difference-kinds))
            (let ((left (%condition-a-category-entries reference name))
                  (right (%condition-a-category-entries candidate name)))
              (loop for index below (max (length left) (length right))
                    for left-entry = (nth index left)
                    for right-entry = (nth index right)
                    unless (equalp left-entry right-entry)
                    do (return-from %condition-a-first-difference
                                    (format nil "~a reference=~s candidate=~s"
                                            name left-entry right-entry)))))))
      (if difference-kinds
          (string-downcase (symbol-name (first difference-kinds)))
        "none"))
    )
  (progn
    (defun %condition-a-complete-utf8-prefix-length (octets)
      (let ((octet-count (length octets)))
        (when (zerop octet-count)
          (return-from %condition-a-complete-utf8-prefix-length 0))
        (let ((sequence-start
               (loop for position downfrom (1- octet-count)
                     above (max -1 (- octet-count 4))
                     unless (<= #x80 (aref octets position) #xbf)
                     return position)))
          (unless sequence-start
            (return-from %condition-a-complete-utf8-prefix-length octet-count))
          (let* ((lead (aref octets sequence-start))
                 (continuation-count (- octet-count sequence-start 1))
                 (expected-continuations
                  (cond
                   ((<= #xc2 lead #xdf) 1)
                   ((<= #xe0 lead #xef) 2)
                   ((<= #xf0 lead #xf4) 3)
                   (t 0))))
            (if (< continuation-count expected-continuations)
                sequence-start
              octet-count)))))
    (defun %condition-a-live-grid (octets rows columns)
      (handler-case
          (%route-grid
           (subseq octets 0 (%condition-a-complete-utf8-prefix-length octets))
           rows columns)
        (ansi-parse-error (condition)
          (if (member (ansi-parse-error-reason condition)
                      '("incomplete escape sequence"
                        "incomplete escape selector"
                        "unterminated CSI sequence"
                        "unterminated control string")
                      :test #'string=)
              nil
            (error condition)))))
    (defun %condition-a-capture
        (program arguments rows columns rate timeout-seconds environment raw-path)
      (let* ((session (launch-e2e-process program arguments rows columns
                                          :environment environment
                                          :timeout-seconds timeout-seconds))
             (process (e2e-session-process session))
             (raw (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
             (first-frame-seconds
              (* *condition-a-first-frame-factor* (/ rate 1000d0)))
             (previous-projection nil)
             (stable-samples 0))
        (unwind-protect
            (loop
             (%route-read-interval session raw *condition-a-observation-seconds*)
             (let* ((grid (%condition-a-live-grid raw rows columns))
                    (projection (and grid (project-criterion-grid grid)))
                    (process-frame (and grid (find-if
                                              (lambda (frame)
                                                (search "Processes"
                                                        (criterion-row-text grid (first frame))
                                                        :test #'char-equal))
                                              (criterion-projection-frames grid))))
                    (process-header-ready-p
                     (and process-frame
                          (or (<= (- (second process-frame) (first process-frame)) 2)
                              (plusp
                               (length
                                (%condition-a-category-entries projection "process-header")))
                              (loop for row from (first process-frame) to (second process-frame)
                                    thereis (search "No data"
                                                    (criterion-row-text grid row)
                                                    :test #'char-equal))))))
               (progn
                 (when (null grid)
                   (setf previous-projection nil
                         stable-samples 0))
                 (when (and process-header-ready-p
                            (%pty-visible-screen-output-p raw)
                            (plusp (length (criterion-projection-frames grid))))
                   (if (equalp previous-projection projection)
                       (incf stable-samples)
                     (setf stable-samples 0))
                   (setf previous-projection projection)
                   (when (and (>= (%elapsed-seconds session (%now))
                                  first-frame-seconds)
                              (>= stable-samples 2))
                     (unless (sb-ext:process-alive-p process)
                       (error "~a exited before its terminal frame stabilized."
                              program))
                     (return grid))))))
          (unwind-protect
              (when (plusp (length raw))
                (%write-octets raw-path raw))
            (close-e2e-session session))))))



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
                                      "process_header_cells" "process_header_sgr"
                                      "differences" "first_difference")
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
                                    height width rate timeout-seconds
                                    environment reference-path)
              for candidate-grid =
              (%condition-a-capture ntop (list "--rate"
                                               (format nil "~dms" rate))
                                    height width rate timeout-seconds
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
                                              :title-cells :title-sgr
                                              :process-header-cells :process-header-sgr))
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
      (if (zerop mismatch-count) 0 1))))

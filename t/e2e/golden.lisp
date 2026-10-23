(in-package #:ntop.e2e)

(defparameter *frame-quiet-seconds*
  0.3d0)

(defun %make-e2e-temporary-directory ()
  (loop for
    directory
    =
    (merge-pathnames
     (format nil "ntop-e2e-~d-~d/" (get-universal-time) (random 1000000000))
     (uiop:temporary-directory))
    unless
    (probe-file directory)
    do
    (ensure-directories-exist (merge-pathnames ".claim" directory))
    (let ((claim
           (open (merge-pathnames ".claim" directory)
                 :direction
                 :output
                 :if-exists
                 nil
                 :if-does-not-exist
                 :create)))
      (when claim
        (close claim)
        (return directory)))))

(defun %capture-btm-criterion (btm config-path
                                   cols
                                   rows
                                   rate
                                   timeout-seconds
                                   raw-path)
  (let ((session
         (launch-e2e-process btm
                             (list "-C" (namestring config-path))
                             rows
                             cols
                             :environment
                             (cons "TERM=xterm-256color"
                                   (remove-if
                                    (lambda (entry)
                                      (and (>= (length entry) 5)
                                           (string= entry "TERM=" :end1 5)))
                                    (sb-ext:posix-environ)))
                             :timeout-seconds
                             timeout-seconds)))
    (unwind-protect
        (loop with
          raw-output
          =
          (make-array 0
                      :element-type
                      '(unsigned-byte 8)
                      :adjustable
                      t
                      :fill-pointer
                      0)
          with
          previous-view
          =
          nil
          do
          (multiple-value-bind (frame-octets)
              (pty-read-until-idle session
                                   :quiet-seconds
                                   *frame-quiet-seconds*
                                   :first-frame-not-before
                                   (* 3d0 (/ rate 1000d0))
                                   :timeout-seconds
                                   timeout-seconds)
            (%append-octets raw-output frame-octets)
            (let* ((grid (make-terminal-grid :rows rows :cols cols))
                   (view
                    (parse-ansi
                     (sb-ext:octets-to-string raw-output
                                              :external-format
                                              :utf-8)
                     grid
                     (lambda (value)
                       (declare (ignore value))
                       (project-criterion-grid grid))
                     (lambda (condition)
                       (error condition)))))
              (when
                  (and previous-view
                       (criterion-projections-equal-p previous-view view))
                (%write-octets raw-path raw-output)
                (return view))
              (setf previous-view view))))
      (close-e2e-session session))))

(defun capture-btm-goldens (&key (output "t/e2e/goldens/")
                                 (btm "btm")
                                 (rate 700)
                                 (timeout-seconds 12d0))
  (unless (and (integerp rate) (plusp rate))
    (error "RATE must be a positive integer."))
  (let* ((minimum-frame-seconds (* 3d0 (/ rate 1000d0)))
         (minimum-timeout (+ minimum-frame-seconds *frame-quiet-seconds*)))
    (unless (and (realp timeout-seconds) (> timeout-seconds minimum-timeout))
      (error
       "TIMEOUT-SECONDS must exceed the first-frame delay and quiet interval."))
    (let* ((temporary-root (%make-e2e-temporary-directory))
           (config-path (merge-pathnames "btm-config.toml" temporary-root))
           (staging-directory (merge-pathnames "goldens/" temporary-root))
           (output-directory (uiop:ensure-directory-pathname (pathname output)))
           (dimensions (golden-dimensions))
           (golden-count (* (length *golden-widths*) (length *golden-heights*)))
           (first-pass (make-array golden-count)))
      (ensure-directories-exist (merge-pathnames ".staging" staging-directory))
      (with-open-file
          (stream config-path
                  :direction
                  :output
                  :if-exists
                  :supersede
                  :if-does-not-exist
                  :create
                  :external-format
                  :utf-8)
        (write-string (btm-config-string rate) stream))
      (unless (= (length dimensions) golden-count)
        (error "The golden dimension table has an unexpected size."))
      (loop for
        run-number
        from
        1
        to
        2
        do
        (loop for
          (width height)
          in
          dimensions
          for
          index
          from
          0
          for
          raw-path
          =
          (merge-pathnames
           (format nil "run-~d-~d-~d.ansi" run-number width height)
           temporary-root)
          for
          view
          =
          (%capture-btm-criterion btm
                                  config-path
                                  width
                                  height
                                  rate
                                  timeout-seconds
                                  raw-path)
          do
          (if (= run-number 1)
              (progn
                (setf (aref first-pass index) view)
                (with-open-file
                    (stream
                     (merge-pathnames (format nil "~d-~d.json" width height)
                                      staging-directory)
                     :direction
                     :output
                     :if-exists
                     :supersede
                     :if-does-not-exist
                     :create
                     :external-format
                     :utf-8)
                  (json-kit:write-json view stream)
                  (terpri stream)))
              (unless (equalp view (aref first-pass index))
                (error
                 "btm criterion-A projections differed at width ~d and height ~d."
                 width
                 height)))))
      (ensure-directories-exist (merge-pathnames ".goldens" output-directory))
      (loop for
        (width height)
        in
        dimensions
        for
        name
        =
        (format nil "~d-~d.json" width height)
        do
        (uiop:copy-file (merge-pathnames name staging-directory)
                        (merge-pathnames name output-directory)))
      (format t "Validated ~d btm golden projections.~%" golden-count)
      0)))

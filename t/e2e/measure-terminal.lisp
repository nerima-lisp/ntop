(in-package #:ntop.e2e)

(defun %btm-environment ()
  (cons "TERM=xterm-256color"
        (remove-if
         (lambda (entry)
           (and (>= (length entry) 5) (string= entry "TERM=" :end1 5)))
         (sb-ext:posix-environ))))

(defun %write-btm-config (directory run rate)
  (let ((path (merge-pathnames (format nil "btm-run-~d.toml" run) directory)))
    (%write-text path (btm-config-string rate))
    path))

(defun %launch-btm (btm config rows columns timeout)
  (launch-e2e-process btm
                      (list "-C" (namestring config))
                      rows
                      columns
                      :environment
                      (%btm-environment)
                      :timeout-seconds
                      timeout))

(defun %read-btm-frame (session rate timeout)
  (pty-read-until-idle session
                       :quiet-seconds
                       *measurement-frame-quiet-seconds*
                       :first-frame-not-before
                       (* *measurement-first-frame-factor* (/ rate 1000d0))
                       :timeout-seconds
                       timeout))

(defun %parse-grid (octets rows columns)
  (let ((grid (make-terminal-grid :rows rows :cols columns)))
    (parse-ansi (%octets-to-text octets)
                grid
                (lambda (value)
                  value)
                (lambda (condition)
                  (error condition)))))

(defun %grid-signature (grid)
  (list (grid-height grid)
        (grid-width grid)
        (loop for
          row
          below
          (grid-height grid)
          append
          (loop for
            column
            below
            (grid-width grid)
            for
            cell
            =
            (grid-cell-at grid row column)
            collect
            (list (cell-character cell)
                  (cell-foreground cell)
                  (cell-background cell)
                  (cell-bold-p cell))))))

(defun %first-grid-and-output (session rows columns rate timeout)
  (multiple-value-bind (octets last-byte quiet-complete)
      (%read-btm-frame session rate timeout)
    (declare (ignore last-byte quiet-complete))
    (values (%parse-grid octets rows columns)
            (make-array (length octets)
                        :element-type
                        '(unsigned-byte 8)
                        :adjustable
                        t
                        :fill-pointer
                        0
                        :initial-contents
                        octets))))

(defun %run-n4-once (directory btm rows columns rate timeout run)
  (let* ((config (%write-btm-config directory run rate))
         (session (%launch-btm btm config rows columns timeout))
         (samples nil))
    (unwind-protect
        (multiple-value-bind (grid cumulative)
            (%first-grid-and-output session rows columns rate timeout)
          (let ((previous (%grid-signature grid)))
            (dotimes (index *measurement-interaction-samples*)
              (let* ((number (1+ index))
                     (key-case
                       (aref *measurement-key-cycle*
                             (mod index (length *measurement-key-cycle*))))
                     (sent-at
                       (pty-send-input session
                                       (getf key-case :octets)
                                       :timeout-seconds
                                       timeout)))
                (multiple-value-bind (octets last-byte quiet)
                    (pty-read-until-idle
                     session
                     :quiet-seconds
                     *measurement-frame-quiet-seconds*
                     :timeout-seconds
                     timeout)
                  (%write-octets
                   (merge-pathnames
                    (format nil "n4-run-~d-sample-~2,'0d.ansi" run number)
                    directory)
                   octets)
                  (%append-octets cumulative octets)
                  (let ((current
                          (%grid-signature
                           (%parse-grid cumulative rows columns))))
                    (unless (and last-byte
                                 (> last-byte sent-at)
                                 (not (equalp previous current)))
                      (%measurement-unsupported
                       "N4 key sample ~d had no visible change; artifacts: ~a"
                       number
                       (namestring directory)))
                    (push (list :sample
                                number
                                :key
                                (getf key-case :key)
                                :sent-at-seconds
                                sent-at
                                :last-byte-seconds
                                last-byte
                                :key-to-last-byte-seconds
                                (- last-byte sent-at)
                                :quiet-complete-seconds
                                quiet
                                :key-to-quiet-complete-seconds
                                (- quiet sent-at))
                          samples)
                    (setf previous current))))))
          (%write-samples directory "n4" run (nreverse samples)))
      (close-e2e-session session))))

(defun %run-n4 (directory btm rows columns rate runs duration timeout)
  (declare (ignore duration))
  (loop for
    run
    from
    1
    to
    runs
    do
    (%run-n4-once directory btm rows columns rate timeout run)))

(defun %run-n5-once (directory btm rows columns rate timeout run)
  (let* ((config (%write-btm-config directory run rate))
         (session (%launch-btm btm config rows columns timeout))
         (samples nil))
    (unwind-protect
        (multiple-value-bind (grid cumulative)
            (%first-grid-and-output session rows columns rate timeout)
          (declare (ignore grid))
          (dotimes (index *measurement-interaction-samples*)
            (let* ((number (1+ index))
                   (target-rows
                    (if (oddp number)
                        (1+ rows)
                        rows))
                   (target-columns
                    (if (oddp number)
                        (1+ columns)
                        columns))
                   (resized-at
                    (pty-resize-session session
                                        target-rows
                                        target-columns
                                        :timeout-seconds
                                        timeout)))
              (multiple-value-bind (octets last-byte quiet)
                  (pty-read-until-idle session
                                       :quiet-seconds
                                       *measurement-frame-quiet-seconds*
                                       :timeout-seconds
                                       timeout)
                (unless
                    (and last-byte
                         (> last-byte resized-at)
                         (plusp (length octets)))
                  (%measurement-unsupported
                   "N5 resize sample ~d had no post-resize bytes; artifacts: ~a"
                   number
                   (namestring directory)))
                (%write-octets
                 (merge-pathnames
                  (format nil "n5-run-~d-sample-~2,'0d.ansi" run number)
                  directory)
                 octets)
                (%append-octets cumulative octets)
                (%parse-grid cumulative target-rows target-columns)
                (push
                 (list :sample
                       number
                       :rows
                       target-rows
                       :columns
                       target-columns
                       :resize-at-seconds
                       resized-at
                       :last-byte-seconds
                       last-byte
                       :resize-to-last-byte-seconds
                       (- last-byte resized-at)
                       :quiet-complete-seconds
                       quiet
                       :resize-to-quiet-complete-seconds
                       (- quiet resized-at))
                 samples))))
          (%write-samples directory "n5" run (nreverse samples)))
      (close-e2e-session session))))

(defun %run-n5 (directory btm rows columns rate runs duration timeout)
  (declare (ignore duration))
  (loop for
    run
    from
    1
    to
    runs
    do
    (%run-n5-once directory btm rows columns rate timeout run)))

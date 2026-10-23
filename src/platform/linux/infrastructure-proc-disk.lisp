(in-package #:ntop.platform.linux)

(defparameter *linux-mount-escapes*
  '(("040" . 32) ("011" . 9) ("012" . 10) ("134" . 92)))

(defparameter *linux-diskstats-sector-indexes*
  '((:read-sectors . 5) (:written-sectors . 9)))

(defun decode-linux-mount-field (field)
  (with-output-to-string (output)
    (loop for index from 0 below (length field)
          for code = (char-code (char field index))
          do (if (and (= code 92) (<= (+ index 3) (1- (length field))))
                 (let* ((escape (subseq field (1+ index) (+ index 4)))
                        (replacement
                         (cdr
                          (assoc escape *linux-mount-escapes* :test #'string=))))
                   (if replacement
                       (progn
                         (write-char (code-char replacement) output)
                         (incf index 3))
                       (write-char (char field index) output)))
                 (write-char (char field index) output)))))

(defun parse-proc-mounts (text on-value on-unavailable)
  (declare (ignore on-unavailable))
  (funcall on-value
           (loop for line in (split-linux-lines text)
                 for fields = (split-linux-fields line)
                 when (>= (length fields) 6)
                   collect (list :device
                                 (unless (string= (first fields) "none")
                                   (first fields))
                                 :mount-point
                                 (decode-linux-mount-field (second fields))
                                 :type
                                 (third fields)
                                 :options
                                 (fourth fields)
                                 :dump
                                 (parse-linux-integer (fifth fields))
                                 :pass-number
                                 (parse-linux-integer (sixth fields))))))

(define-linux-file-reader read-proc-mounts "proc/mounts" parse-proc-mounts)

(defun parse-proc-diskstats-line (line)
  (let* ((fields (split-linux-fields line))
         (name (third fields))
         (read-sectors
          (parse-linux-integer
           (nth (cdr (assoc :read-sectors *linux-diskstats-sector-indexes*))
                fields)))
         (written-sectors
          (parse-linux-integer
           (nth (cdr (assoc :written-sectors *linux-diskstats-sector-indexes*))
                fields))))
    (when (and name read-sectors written-sectors)
      (list :name
            name
            :read-sectors
            read-sectors
            :written-sectors
            written-sectors
            :read-bytes
            (* read-sectors 512)
            :written-bytes
            (* written-sectors 512)))))

(defun parse-proc-diskstats (text on-value on-unavailable)
  (declare (ignore on-unavailable))
  (funcall on-value
           (loop for line in (split-linux-lines text)
                 for entry = (parse-proc-diskstats-line line)
                 when entry
                   collect entry)))

(define-linux-file-reader read-proc-diskstats
                          "proc/diskstats"
                          parse-proc-diskstats)

#+linux
(sb-alien:define-alien-type linux-statvfs
  (sb-alien:struct statvfs
    (f-bsize sb-alien:unsigned-long)
    (f-frsize sb-alien:unsigned-long)
    (f-blocks sb-alien:unsigned-long-long)
    (f-bfree sb-alien:unsigned-long-long)
    (f-bavail sb-alien:unsigned-long-long)
    (f-files sb-alien:unsigned-long-long)
    (f-ffree sb-alien:unsigned-long-long)
    (f-favail sb-alien:unsigned-long-long)
    (f-fsid sb-alien:unsigned-long)
    (f-flag sb-alien:unsigned-long)
    (f-namemax sb-alien:unsigned-long)
    (f-type sb-alien:unsigned)
    (f-spare (array sb-alien:int 5))))

#+linux
(sb-alien:define-alien-routine ("statvfs" %linux-statvfs) sb-alien:int
  (path sb-alien:c-string)
  (buffer (* linux-statvfs)))

#+linux
(defun call-linux-statvfs (pathname)
  (sb-alien:with-alien ((stats linux-statvfs))
    (when (zerop (%linux-statvfs (namestring pathname)
                                 (sb-alien:addr stats)))
      (list :block-size (sb-alien:slot stats 'f-bsize)
            :fragment-size (sb-alien:slot stats 'f-frsize)
            :blocks (sb-alien:slot stats 'f-blocks)
            :free-blocks (sb-alien:slot stats 'f-bfree)
            :available-blocks (sb-alien:slot stats 'f-bavail)
            :files (sb-alien:slot stats 'f-files)
            :free-files (sb-alien:slot stats 'f-ffree)
            :available-files (sb-alien:slot stats 'f-favail)
            :name-max (sb-alien:slot stats 'f-namemax)))))

(defparameter *linux-statvfs-function*
  #+linux #'call-linux-statvfs
  #-linux nil)

(defparameter *linux-statvfs-fields*
  '(:block-size :fragment-size
                :blocks
                :free-blocks
                :available-blocks
                :files
                :free-files
                :available-files
                :name-max))

(defun linux-mount-point-pathname (mount-point)
  (let* ((supplied-pathname (pathname mount-point))
         (pathname
          (unless (wild-pathname-p supplied-pathname)
            (uiop:ensure-directory-pathname supplied-pathname)))
         (directory (and pathname (pathname-directory pathname))))
    (when
        (and pathname
             *linux-root*
             (eq (first directory) :absolute)
             (not (member :up directory))
             (not (member :back directory)))
      (merge-pathnames
       (make-pathname :directory (cons :relative (rest directory)))
       *linux-root*))))

(defun parse-linux-statvfs (values on-value on-unavailable)
  (let ((fields
         (mapcar
          (lambda (key)
            (getf values key :missing))
          *linux-statvfs-fields*)))
    (cond
      ((member :missing fields) (funcall on-unavailable :missing-field))
      ((not
        (every
         (lambda (value)
           (and (integerp value) (not (minusp value))))
         fields)) (funcall on-unavailable :invalid-data))
      (t
       (let ((fragment-size (getf values :fragment-size)))
         (funcall on-value
                  (append values
                          (list :total-bytes
                                (* fragment-size (getf values :blocks))
                                :free-bytes
                                (* fragment-size (getf values :free-blocks))
                                :available-bytes
                                (* fragment-size
                                   (getf values :available-blocks))))))))))

(defun read-linux-statvfs (mount-point on-value on-unavailable)
  (let ((pathname (linux-mount-point-pathname mount-point)))
    (cond
      ((null *linux-root*) (funcall on-unavailable :root-unavailable))
      ((null pathname) (funcall on-unavailable :invalid-path))
      ((null *linux-statvfs-function*) (funcall on-unavailable :unsupported))
      (t
       (let ((values (funcall *linux-statvfs-function* pathname)))
         (if values
             (parse-linux-statvfs values on-value on-unavailable)
             (funcall on-unavailable :unavailable)))))))

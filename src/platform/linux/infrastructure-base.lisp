(in-package #:ntop.platform.linux)

(defvar *linux-root*
  #+linux #p"/"
  #-linux nil)

(defconstant +linux-file-character-limit+
  1048576)

(defmacro with-linux-root (root &body body)
  `(let ((*linux-root* (uiop:ensure-directory-pathname ,root)))
     ,@body))

(defmacro define-linux-file-reader (name relative-path parser)
  `(defun ,name (on-value on-unavailable)
     (read-linux-file ,relative-path
                      (lambda (text)
                        (,parser text on-value on-unavailable))
                      on-unavailable)))

(defmacro define-linux-process-reader (name file parser)
  `(defun ,name (pid on-value on-unavailable)
     (if (and (integerp pid) (plusp pid))
         (read-linux-file (format nil "proc/~D/~A" pid ,file)
                          (lambda (text)
                            (,parser text on-value on-unavailable))
                          on-unavailable)
         (funcall on-unavailable :invalid-pid))))

(defun linux-pathname (relative-path)
  (let* ((pathname (pathname relative-path))
         (directory (pathname-directory pathname)))
    (when
        (and *linux-root*
             (or (null directory) (eq (first directory) :relative))
             (not (member :back directory))
             (not (member :up directory)))
      (merge-pathnames pathname *linux-root*))))
(defun linux-errno-unavailability-reason (errno)
  (cond
    ((eql errno sb-posix:enoent) :not-present)
    ((or (eql errno sb-posix:eacces)
         (eql errno sb-posix:eperm))
     :permission-denied)
    (t :read-failed)))

(defun read-bounded-linux-text (stream limit)
  (let ((text
         (with-output-to-string (output)
           (loop for index from 0
                 for character = (read-char stream nil nil)
                 while character
                 do (progn
                      (when (= index limit)
                        (return-from read-bounded-linux-text
                          (values nil :too-large)))
                      (write-char character output))))))
    (values text nil)))

(defun read-linux-file (relative-path on-value
                                      on-unavailable
                                      &key
                                      (limit +linux-file-character-limit+))
  (let ((pathname (linux-pathname relative-path)))
    (cond
      ((null *linux-root*) (funcall on-unavailable :root-unavailable))
      ((null pathname) (funcall on-unavailable :invalid-path))
      ((not (and (integerp limit) (plusp limit)))
       (funcall on-unavailable :invalid-limit))
      (t
       (let ((descriptor nil)
             (file-stream nil))
         (unwind-protect
              (handler-case
                  (progn
                    (setf descriptor
                          (sb-posix:open (namestring pathname)
                                         sb-posix:o-rdonly))
                    (setf file-stream
                          (sb-sys:make-fd-stream
                           descriptor
                           :input t
                           :element-type 'character
                           :external-format :utf-8
                           :auto-close t
                           :pathname pathname))
                    (setf descriptor nil)
                    (with-open-stream (stream file-stream)
                      (multiple-value-bind (text failure)
                          (read-bounded-linux-text stream limit)
                        (if failure
                            (funcall on-unavailable failure)
                            (funcall on-value text)))))
                (sb-posix:syscall-error (condition)
                  (funcall on-unavailable
                           (linux-errno-unavailability-reason
                            (sb-posix:syscall-errno condition))))
                (file-error ()
                  (funcall on-unavailable :read-failed))
                (stream-error ()
                  (funcall on-unavailable :invalid-encoding)))
           (when descriptor
             (sb-posix:close descriptor))))))))

(defun linux-directory-entry-name (pathname)
  (or
   (let ((name (file-namestring pathname)))
     (unless (zerop (length name))
       name))
   (first (last (pathname-directory pathname)))))

(defun read-linux-directory (relative-path on-value on-unavailable)
  (let ((pathname (linux-pathname relative-path)))
    (cond
      ((null *linux-root*) (funcall on-unavailable :root-unavailable))
      ((null pathname) (funcall on-unavailable :invalid-path))
      (t
       (handler-case
           (let* ((directory (sb-posix:opendir (namestring pathname)))
                  (entries
                    (unwind-protect
                         (let* ((directory-pathname
                                 (uiop:ensure-directory-pathname pathname))
                                (pathnames
                                  (append (uiop:directory-files
                                           directory-pathname)
                                          (uiop:subdirectories
                                           directory-pathname))))
                           (sort
                            (remove-duplicates
                             (mapcar #'linux-directory-entry-name pathnames)
                             :test
                             #'string=)
                            #'string<))
                      (sb-posix:closedir directory))))
             (funcall on-value entries))
         (sb-posix:syscall-error (condition)
           (funcall on-unavailable
                    (linux-errno-unavailability-reason
                     (sb-posix:syscall-errno condition))))
         (file-error ()
           (funcall on-unavailable :read-failed)))))))

(defun split-linux-fields (text)
  (let ((fields nil)
        (start nil))
    (dotimes (index (length text))
      (if (find (char text index) '(#\Space #\Tab #\Newline #\Return))
          (when start
            (push (subseq text start index) fields)
            (setf start nil))
          (unless start
            (setf start index))))
    (when start
      (push (subseq text start) fields))
    (nreverse fields)))

(defun split-linux-lines (text)
  (with-input-from-string (input text)
    (loop for line = (read-line input nil nil)
          while line
          collect line)))

(defun ascii-linux-digit-p (character)
  (and (char<= #\0 character #\9)))

(defun linux-integer-token-p (text)
  (let ((start
         (if (and (plusp (length text)) (find (char text 0) '(#\+ #\-)))
             1
             0)))
    (and (< start (length text))
         (loop for index from start below (length text)
               always (ascii-linux-digit-p (char text index))))))

(defun parse-linux-integer (text)
  (when (linux-integer-token-p text)
    (parse-integer text)))

(defun parse-linux-decimal (text)
  (let* ((first-character (and (plusp (length text)) (char text 0)))
         (negative-p (eql first-character #\-))
         (signed-p (member first-character (quote (#\+ #\-))))
         (unsigned-text
          (if signed-p
              (subseq text 1)
              text))
         (point (position #\. unsigned-text)))
    (when (and point (not (position #\. unsigned-text :start (1+ point))))
      (let* ((whole-text (subseq unsigned-text 0 point))
             (fraction-text (subseq unsigned-text (1+ point)))
             (whole
              (if (zerop (length whole-text))
                  0
                  (and (every (function ascii-linux-digit-p) whole-text)
                       (parse-linux-integer whole-text))))
             (fraction
              (and (plusp (length fraction-text))
                   (every (function ascii-linux-digit-p) fraction-text)
                   (parse-linux-integer fraction-text))))
        (when (and whole fraction)
          (let ((value (+ whole (/ fraction (expt 10 (length fraction-text))))))
            (if negative-p
                (- value)
                value)))))))

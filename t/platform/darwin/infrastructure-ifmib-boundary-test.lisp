(in-package #:ntop.test)

(defparameter *network-netstat-counter-fields*
  '((:rx-bytes "Ibytes")
    (:tx-bytes "Obytes")
    (:rx-packets "Ipkts")
    (:tx-packets "Opkts")))

(defun %split-netstat-fields (line)
  (let ((fields nil)
        (start nil))
    (loop for offset from 0 to (length line)
          for separator = (or (= offset (length line))
                              (find (char line offset)
                                    '(#\Space #\Tab #\Return #\Newline)))
          do (cond
               ((and separator start)
                (push (subseq line start offset) fields)
                (setf start nil))
               ((and (not separator) (null start))
                (setf start offset))))
    (nreverse fields)))

(defun %netstat-column-positions (fields)
  (let* ((last-position (1- (length fields)))
         (name-position (position "Name" fields :test (function string-equal)))
         (counter-positions
           (loop for field in *network-netstat-counter-fields*
                 collect
                 (cons (first field)
                       (let ((position (position (second field)
                                                  fields
                                                  :test (function string-equal))))
                         (and position (- last-position position)))))))
    (when (and name-position
               (every (lambda (column) (integerp (cdr column)))
                      counter-positions))
      (cons (cons :name name-position) counter-positions))))

(defun %parse-netstat-counter (text)
  (when (and text
             (plusp (length text))
             (every #'digit-char-p text))
    (parse-integer text)))

(defun %netstat-field-at-end-offset (fields offset)
  (let ((position (- (length fields) offset 1)))
    (and (<= 0 position)
         (< position (length fields))
         (nth position fields))))

(defun %parse-netstat-output (output)
  (let ((lines (with-input-from-string (stream output)
                 (loop for line = (read-line stream nil nil)
                       while line
                       collect line)))
        (columns nil))
    (dolist (line lines)
      (unless columns
        (setf columns (%netstat-column-positions
                       (%split-netstat-fields line)))))
    (unless columns
      (error "netstat -ib returned no recognizable interface header."))
    (let* ((name-position (cdr (assoc :name columns)))
           (counter-columns (remove :name columns :key (function car)))
           (interfaces (make-hash-table :test (quote equal))))
      (dolist (line lines)
        (let* ((fields (%split-netstat-fields line))
               (name
                 (let ((token (nth name-position fields)))
                   (if (and token
                            (plusp (length token))
                            (char= (char token (1- (length token))) #\*))
                       (subseq token 0 (1- (length token)))
                       token)))
               (counters
                 (loop for column in counter-columns
                       collect
                       (cons (car column)
                             (%parse-netstat-counter
                              (%netstat-field-at-end-offset
                               fields
                               (cdr column))))))
               (counter-plist
                 (loop for counter in counters
                       append (list (car counter) (cdr counter))))
               (record (append (list :name name) counter-plist)))
          (when (and name
                     (not (string-equal name "Name"))
                     (every (lambda (counter) (integerp (cdr counter)))
                            counters)
                     (not (gethash name interfaces)))
            (setf (gethash name interfaces) record))))
      (when (zerop (hash-table-count interfaces))
        (error "netstat -ib returned no interface counters."))
      interfaces)))

(defun %network-netstat-snapshot ()
  (let* ((result (host-kit:run-program "netstat"
                                       (list "-ib")
                                       :timeout 5
                                       :max-output-characters 131072))
         (output (host-kit:process-result-stdout result)))
    (unless (and (zerop (host-kit:process-result-exit-code result))
                 (not (host-kit:process-result-stdout-truncated-p result))
                 (not (host-kit:process-result-stderr-truncated-p result)))
      (error "netstat -ib failed or exceeded its output limit."))
    (%parse-netstat-output output)))

(defun %counter-within-netstat-bracket-p (value before after)
  (<= (min before after) value (max before after)))

(cl-weave:describe "Darwin ifmib boundary"
  (cl-weave:it-run-if (member :darwin *features*)
    "keeps each interface counter within the netstat -ib bracket"
    (let ((before (%network-netstat-snapshot))
          (after nil)
          (outcome nil)
          (interfaces nil)
          (unavailable-reason nil)
          (callback-count 0))
      (unwind-protect
           (ntop.network.infrastructure:read-network-interface-data
            (make-instance (quote ntop.network.infrastructure:network-collector)
                           :clock nil)
            (lambda (value)
              (incf callback-count)
              (setf outcome :value
                    interfaces value))
            (lambda (reason)
              (incf callback-count)
              (setf outcome :unavailable
                    unavailable-reason reason)))
        (setf after (%network-netstat-snapshot)))
      (cl-weave:expect callback-count :to-be 1)
      (cl-weave:expect outcome :to-be :value)
      (cl-weave:expect (not (null interfaces)) :to-be t)
      (cl-weave:expect (> (hash-table-count before) 0) :to-be t)
      (cl-weave:expect (> (hash-table-count after) 0) :to-be t)
      (cl-weave:expect unavailable-reason :to-be nil)
      (let ((compared 0))
        (dolist (interface interfaces)
          (let* ((name (getf interface :name))
                 (before-counters (gethash name before))
                 (after-counters (gethash name after)))
            (cl-weave:expect (not (null before-counters)) :to-be t)
            (cl-weave:expect (not (null after-counters)) :to-be t)
            (incf compared)
            (dolist (field (quote (:rx-bytes :tx-bytes :rx-packets :tx-packets)))
              (cl-weave:expect
               (%counter-within-netstat-bracket-p
                (getf interface field)
                (getf before-counters field)
                (getf after-counters field))
               :to-be
               t))))
        (cl-weave:expect (> compared 0) :to-be t)))))

(cl-weave:describe "netstat interface parser"
  (cl-weave:it "handles blank address fields and netstat interface markers"
    (let* ((snapshot
             (%parse-netstat-output
              (format nil
                      "~A~%~A~%~A~%~A~%"
                      "Name Mtu Network Address Ipkts Ierrs Ibytes Opkts Oerrs Obytes Coll"
                      "test0* 16384 <Link#1> 2 0 20 3 0 30 0"
                      "test1 1500 <Link#2> addr 5 0 50 7 0 70 0"
                      "test0* 16384 <Link#1> 4 0 40 5 0 50 0")))
           (test0 (gethash "test0" snapshot))
           (test1 (gethash "test1" snapshot)))
      (cl-weave:expect (hash-table-count snapshot) :to-be 2)
      (cl-weave:expect (getf test0 :rx-bytes) :to-be 20)
      (cl-weave:expect (getf test0 :tx-bytes) :to-be 30)
      (cl-weave:expect (getf test0 :rx-packets) :to-be 2)
      (cl-weave:expect (getf test0 :tx-packets) :to-be 3)
      (cl-weave:expect (getf test1 :rx-bytes) :to-be 50)
      (cl-weave:expect (getf test1 :tx-bytes) :to-be 70)
      (cl-weave:expect (getf test1 :rx-packets) :to-be 5)
      (cl-weave:expect (getf test1 :tx-packets) :to-be 7))))

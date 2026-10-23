(in-package #:ntop.test)

(defun %write-little-endian-unsigned (octets offset width value)
  (dotimes (index width)
    (setf (aref octets (+ offset index))
          (ldb (byte 8 (* index 8)) value)))
  octets)

(defun %make-ifmibdata-octets ()
  (let ((octets (make-array 180
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (loop for character across "synthetic0"
          for offset from 0
          do (setf (aref octets offset) (char-code character)))
    (%write-little-endian-unsigned octets 116 8 #x0102030405060708)
    (%write-little-endian-unsigned octets 124 8 #x1112131415161718)
    (%write-little-endian-unsigned octets 76 8 #x2122232425262728)
    (%write-little-endian-unsigned octets 92 8 #x3132333435363738)))

(cl-weave:describe "ifmibdata decoder"
  (cl-weave:it "decodes the fixed name and all four 64-bit counters"
    (let ((data
            (ntop.platform.darwin:decode-ifmibdata
             (%make-ifmibdata-octets))))
      (cl-weave:expect (getf data :name) :to-equal "synthetic0")
      (cl-weave:expect (getf data :rx-bytes) :to-be #x0102030405060708)
      (cl-weave:expect (getf data :tx-bytes) :to-be #x1112131415161718)
      (cl-weave:expect (getf data :rx-packets) :to-be #x2122232425262728)
      (cl-weave:expect (getf data :tx-packets) :to-be #x3132333435363738)))
  (cl-weave:it "rejects an unexpected structure size"
    (cl-weave:expect
     (lambda ()
       (ntop.platform.darwin:decode-ifmibdata
        (make-array 179 :element-type '(unsigned-byte 8)
                    :initial-element 0)))
     :to-throw
     'error)))

(in-package #:ntop.test)

(cl-weave:describe "Darwin statfs decoder"
                   (cl-weave:it-each
                    (("/dev/disk9s1" "/"
                                     "apfs"
                                     4096
                                     1000
                                     300
                                     250
                                     (:device-name "/dev/disk9s1"
                                                   :mount-path
                                                   "/"
                                                   :filesystem-type
                                                   "apfs"
                                                   :block-size
                                                   4096
                                                   :blocks
                                                   1000
                                                   :free-blocks
                                                   300
                                                   :available-blocks
                                                   250)))
                    "decodes statfs fields from ~A"
                    (device mount
                            filesystem
                            block-size
                            blocks
                            free
                            available
                            expected)
                    (let* ((entry
                            (%make-statfs-entry device
                                                mount
                                                filesystem
                                                block-size
                                                blocks
                                                free
                                                available))
                           (row
                            (first
                             (ntop.platform.darwin:decode-darwin-statfs-buffer
                              entry
                              1))))
                      (cl-weave:expect row :to-equal expected)))
                   (cl-weave:it
                    "decodes multiple entries and permits trailing bytes"
                    (let* ((first-entry
                            (%make-statfs-entry "/dev/disk1"
                                                "/"
                                                "apfs"
                                                4096
                                                10
                                                4
                                                3))
                           (second-entry
                            (%make-statfs-entry "/dev/disk2"
                                                "/data"
                                                "hfs"
                                                1024
                                                20
                                                8
                                                6))
                           (octets
                            (concatenate '(vector (unsigned-byte 8))
                                         first-entry
                                         second-entry
                                         #(9 8))))
                      (cl-weave:expect
                       (mapcar
                        (lambda (row)
                          (getf row :mount-path))
                        (ntop.platform.darwin:decode-darwin-statfs-buffer octets
                                                                          2))
                       :to-equal
                       '("/" "/data")))) (cl-weave:it "returns no rows for a zero count"
                                 (cl-weave:expect
                                  (ntop.platform.darwin:decode-darwin-statfs-buffer
                                   (%make-disk-test-octets nil)
                                   0)
                                  :to-equal
                                  nil)) (cl-weave:it-each ((nil 1) ((1 2 3) 1))
                                      "rejects short statfs input ~S with count ~D"
                                      (octet-values count)
                                      (cl-weave:expect
                                       (lambda ()
                                         (ntop.platform.darwin:decode-darwin-statfs-buffer
                                          (%make-disk-test-octets octet-values)
                                          count))
                                       :to-throw
                                       'error)) (cl-weave:it "rejects a negative statfs count"
                                 (cl-weave:expect
                                  (lambda ()
                                    (ntop.platform.darwin:decode-darwin-statfs-buffer
                                     (%make-disk-test-octets nil)
                                     -1))
                                  :to-throw
                                  'error)) (cl-weave:it "rejects a zero statfs block size"
                                 (let ((entry
                                        (%make-statfs-entry "/dev/disk1"
                                                            "/"
                                                            "apfs"
                                                            0
                                                            10
                                                            4
                                                            3)))
                                   (cl-weave:expect
                                    (lambda ()
                                      (ntop.platform.darwin:decode-darwin-statfs-buffer
                                       entry
                                       1))
                                    :to-throw
                                    'error)))
                   (cl-weave:describe "Darwin volume-space-used decoder"
                                      (cl-weave:it-each
                                       ((0 0) (1 1) (1234567890 1234567890))
                                       "decodes ~D used bytes"
                                       (used-bytes expected)
                                       (cl-weave:expect
                                        (ntop.platform.darwin:decode-darwin-volume-space-used
                                         (%make-volume-space-used-record 12
                                                                         12
                                                                         used-bytes))
                                        :to-be
                                        expected))
                                      (cl-weave:it-property
                                       "preserves unsigned 64-bit used byte counts"
                                       ((used-bytes
                                         (cl-weave:gen-integer :min
                                                               0
                                                               :max
                                                               18446744073709551615)))
                                       (cl-weave:expect
                                        (ntop.platform.darwin:decode-darwin-volume-space-used
                                         (%make-volume-space-used-record 12
                                                                         12
                                                                         used-bytes))
                                        :to-be
                                        used-bytes))
                                      (cl-weave:it-each
                                       ((11 12) (13 12) (12 11))
                                       "rejects record length ~D in a ~D byte buffer"
                                       (declared-size buffer-size)
                                       (cl-weave:expect
                                        (lambda ()
                                          (ntop.platform.darwin:decode-darwin-volume-space-used
                                           (%make-volume-space-used-record
                                            declared-size
                                            buffer-size
                                            42)))
                                        :to-throw
                                        'error))
                                      (cl-weave:it
                                       "rejects a buffer shorter than the length word"
                                       (cl-weave:expect
                                        (lambda ()
                                          (ntop.platform.darwin:decode-darwin-volume-space-used
                                           (%make-disk-test-octets nil)))
                                        :to-throw
                                        'error))))

(cl-weave:describe "Darwin volume attrlist"
                   (cl-weave:it "encodes the requested volume attributes"
  (let ((octets (ntop.platform.darwin::%make-darwin-volume-attrlist)))
    (cl-weave:expect
     (list
      (length octets)
      (ntop.platform.darwin::decode-little-endian-unsigned
       octets :start 0 :end 2)
      (ntop.platform.darwin::decode-little-endian-unsigned
       octets :start 8 :end 12)
      (every #'zerop (subseq octets 12)))
     :to-equal
     (list 24 5 #x80800000 t)))))

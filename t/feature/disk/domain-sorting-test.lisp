(in-package #:ntop.test)

(defun %f5-sort-row (&key disk
                          mount
                          filesystem-type
                          used
                          free
                          total
                          used-percent
                          free-percent
                          read-rate
                          write-rate
                          io-key)
  (list :disk
        disk
        :mount
        mount
        :filesystem-type
        filesystem-type
        :used
        used
        :free
        free
        :total
        total
        :used-percent
        used-percent
        :free-percent
        free-percent
        :read-rate
        read-rate
        :write-rate
        write-rate
        :io-key
        io-key))

(defun %f5-sort-mounts (rows)
  (mapcar
   (lambda (row)
     (getf row :mount))
   rows))

(cl-weave:describe "F5 disk sorting"
                   (cl-weave:it-each
                    ((:disk-ascending :disk :ascending ("/a" "/z"))
                     (:disk-descending :disk :descending ("/z" "/a"))
                     (:mount-ascending :mount :ascending ("/a" "/z"))
                     (:mount-descending :mount :descending ("/z" "/a"))
                     (:filesystem-ascending :filesystem-type
                                            :ascending
                                            ("/a" "/z"))
                     (:filesystem-descending :filesystem-type
                                             :descending
                                             ("/z" "/a"))
                     (:used-ascending :used :ascending ("/a" "/z"))
                     (:used-descending :used :descending ("/z" "/a"))
                     (:free-ascending :free :ascending ("/z" "/a"))
                     (:free-descending :free :descending ("/a" "/z"))
                     (:total-ascending :total :ascending ("/z" "/a"))
                     (:total-descending :total :descending ("/a" "/z"))
                     (:used-percent-ascending :used-percent
                                              :ascending
                                              ("/a" "/z"))
                     (:used-percent-descending :used-percent
                                               :descending
                                               ("/z" "/a"))
                     (:free-percent-ascending :free-percent
                                              :ascending
                                              ("/z" "/a"))
                     (:free-percent-descending :free-percent
                                               :descending
                                               ("/a" "/z"))
                     (:read-rate-ascending :read-rate :ascending ("/a" "/z"))
                     (:read-rate-descending :read-rate :descending ("/z" "/a"))
                     (:write-rate-ascending :write-rate :ascending ("/z" "/a"))
                     (:write-rate-descending :write-rate
                                             :descending
                                             ("/a" "/z"))
                     (:io-key-ascending :io-key :ascending ("/a" "/z"))
                     (:io-key-descending :io-key :descending ("/z" "/a")))
                    "sorts ~A by ~A in ~A order"
                    (case-name sort-key sort-order expected)
                    (let ((rows
                           (list
                            (%f5-sort-row :disk
                                          "disk2"
                                          :mount
                                          "/z"
                                          :filesystem-type
                                          "zfs"
                                          :used
                                          75
                                          :free
                                          25
                                          :total
                                          100
                                          :used-percent
                                          75
                                          :free-percent
                                          25
                                          :read-rate
                                          200
                                          :write-rate
                                          100
                                          :io-key
                                          "disk2")
                            (%f5-sort-row :disk
                                          "disk1"
                                          :mount
                                          "/a"
                                          :filesystem-type
                                          "ext4"
                                          :used
                                          25
                                          :free
                                          75
                                          :total
                                          200
                                          :used-percent
                                          25
                                          :free-percent
                                          75
                                          :read-rate
                                          100
                                          :write-rate
                                          200
                                          :io-key
                                          "disk1"))))
                      (cl-weave:expect
                       (%f5-sort-mounts
                        (ntop.disk.domain:sort-disk-rows rows
                                                         :sort-key
                                                         sort-key
                                                         :sort-order
                                                         sort-order))
                       :to-equal
                       expected)))
                   (cl-weave:it-each
                    ((:disk-ascending :disk :ascending)
                     (:disk-descending :disk :descending)
                     (:mount-ascending :mount :ascending)
                     (:mount-descending :mount :descending)
                     (:filesystem-ascending :filesystem-type :ascending)
                     (:filesystem-descending :filesystem-type :descending)
                     (:used-ascending :used :ascending)
                     (:used-descending :used :descending)
                     (:free-ascending :free :ascending)
                     (:free-descending :free :descending)
                     (:total-ascending :total :ascending)
                     (:total-descending :total :descending)
                     (:used-percent-ascending :used-percent :ascending)
                     (:used-percent-descending :used-percent :descending)
                     (:free-percent-ascending :free-percent :ascending)
                     (:free-percent-descending :free-percent :descending)
                     (:read-rate-ascending :read-rate :ascending)
                     (:read-rate-descending :read-rate :descending)
                     (:write-rate-ascending :write-rate :ascending)
                     (:write-rate-descending :write-rate :descending)
                     (:io-key-ascending :io-key :ascending)
                     (:io-key-descending :io-key :descending))
                    "keeps missing ~A values last with ~A order"
                    (case-name sort-key sort-order)
                    (let* ((missing-row
                            (%f5-sort-row :disk
                                          "disk2"
                                          :mount
                                          "/z"
                                          :filesystem-type
                                          "zfs"
                                          :used
                                          75
                                          :free
                                          25
                                          :total
                                          100
                                          :used-percent
                                          75
                                          :free-percent
                                          25
                                          :read-rate
                                          200
                                          :write-rate
                                          100
                                          :io-key
                                          "disk2"))
                           (present-row
                            (%f5-sort-row :disk
                                          "disk1"
                                          :mount
                                          "/a"
                                          :filesystem-type
                                          "ext4"
                                          :used
                                          25
                                          :free
                                          75
                                          :total
                                          200
                                          :used-percent
                                          25
                                          :free-percent
                                          75
                                          :read-rate
                                          100
                                          :write-rate
                                          200
                                          :io-key
                                          "disk1")))
                      (setf (getf missing-row sort-key) nil)
                      (let ((sorted
                             (ntop.disk.domain:sort-disk-rows
                              (list missing-row present-row)
                              :sort-key
                              sort-key
                              :sort-order
                              sort-order)))
                        (cl-weave:expect (eq (first sorted) present-row)
                                         :to-be
                                         t)
                        (cl-weave:expect (eq (second sorted) missing-row)
                                         :to-be
                                         t))))
                   (cl-weave:it-each
                    ((:ascending :ascending) (:descending :descending))
                    "breaks equal sort values by disk then mount with ~A order"
                    (case-name sort-order)
                    (let ((rows
                           (list
                            (%f5-sort-row :disk "disk2" :mount "/a" :used 1)
                            (%f5-sort-row :disk "disk1" :mount "/z" :used 1)
                            (%f5-sort-row :disk "disk1" :mount "/b" :used 1))))
                      (cl-weave:expect
                       (mapcar
                        (lambda (row)
                          (list (getf row :disk) (getf row :mount)))
                        (ntop.disk.domain:sort-disk-rows rows
                                                         :sort-key
                                                         :used
                                                         :sort-order
                                                         sort-order))
                       :to-equal
                       '(("disk1" "/b") ("disk1" "/z") ("disk2" "/a"))))
                    (cl-weave:it
                     "defaults to disk ascending and leaves the input list intact"
                     (let* ((first-row (%f5-sort-row :disk "disk2" :mount "/z"))
                            (second-row
                             (%f5-sort-row :disk "disk1" :mount "/a"))
                            (rows (list first-row second-row))
                            (sorted (ntop.disk.domain:sort-disk-rows rows)))
                       (cl-weave:expect (%f5-sort-mounts sorted)
                                        :to-equal
                                        '("/a" "/z"))
                       (cl-weave:expect (first rows) :to-be first-row)
                       (cl-weave:expect (second rows) :to-be second-row))))
                   (cl-weave:it
                    "preserves equal rows and rejects unsupported sort inputs"
                    (let* ((first-row
                            (%f5-sort-row :disk "disk1" :mount "/" :used 1))
                           (second-row
                            (%f5-sort-row :disk "disk1" :mount "/" :used 1))
                           (rows (list first-row second-row)))
                      (cl-weave:expect
                       (ntop.disk.domain:sort-disk-rows rows :sort-key :used)
                       :to-equal
                       rows)
                      (cl-weave:expect
                       (lambda ()
                         (ntop.disk.domain:sort-disk-rows rows
                                                          :sort-key
                                                          :unknown))
                       :to-throw
                       'error)
                      (cl-weave:expect
                       (lambda ()
                         (ntop.disk.domain:sort-disk-rows rows
                                                          :sort-order
                                                          :sideways))
                       :to-throw
                       'error)
                      (cl-weave:expect
                       (lambda ()
                         (ntop.disk.domain:sort-disk-rows
                          (list (%f5-sort-row :used 1)
                                (%f5-sort-row :used "one"))
                          :sort-key
                          :used))
                       :to-throw
                       'error))))

(cl-weave:describe "Disk comparator missing values"
                   (cl-weave:it "orders nil values on either side"
                                (cl-weave:expect
                                 (ntop.disk.domain::%compare-disk-values nil
                                                                         "disk1")
                                 :to-be
                                 1)
                                (cl-weave:expect
                                 (ntop.disk.domain::%compare-disk-values "disk1"
                                                                         nil)
                                 :to-be
                                 -1)
                                (cl-weave:expect
                                 (ntop.disk.domain::%compare-disk-values nil
                                                                         nil)
                                 :to-be
                                 0)))

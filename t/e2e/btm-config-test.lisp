(in-package #:ntop.e2e.test)

(defun btm-config-test-contains-p (text fragment)
  (not (null (search fragment text))))

(defun btm-config-test-count (text fragment)
  (loop with position = 0
        for found = (search fragment text :start2 position)
        while found
        count t
        do (setf position (+ found (length fragment)))))

(defun btm-config-test-layout-order-p (text)
  (let ((positions
         (mapcar
          (lambda (type)
            (search (format nil "type = ~s" type) text))
          '("cpu" "mem" "net" "disk" "temp" "proc"))))
    (and (every #'integerp positions)
         (loop for previous in positions
               for next in (cdr positions)
               always (< previous next)))))

(cl-weave:describe "btm configuration projection"
                   (cl-weave:it "expands declarative layout rows"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e:define-btm-layout
                                    ((cpu 2) (mem nil))
                                    ((net 2) (disk 2) (temp nil))
                                    ((proc 2 :default))))
                                 :to-equal
                                 '(quote
                                   (((cpu 2) (mem nil))
                                    ((net 2) (disk 2) (temp nil))
                                    ((proc 2 :default))))))
                   (cl-weave:it-each
                    ((:minimum-rate 1) (:configured-rate 700)
                     (:higher-rate 1200))
                    "renders deterministic flags and layout at ~A"
                    (case-name rate)
                    (declare (ignore case-name))
                    (let ((text (ntop.e2e:btm-config-string rate)))
                      (cl-weave:expect
                       (btm-config-test-contains-p text
                                                   (format nil
                                                           "[flags]~%basic = false~%dot_marker = false~%rate = ~d"
                                                           rate))
                       :to-be
                       t)
                      (cl-weave:expect
                       (btm-config-test-contains-p text
                                                   (format nil
                                                           "[processes]~%case_sensitive = false~%default_grouped = true"))
                       :to-be
                       t)
                      (cl-weave:expect (btm-config-test-count text "[[row]]")
                                       :to-be
                                       3)
                      (cl-weave:expect
                       (btm-config-test-count text "[[row.child]]")
                       :to-be
                       6)
                      (cl-weave:expect (btm-config-test-count text "ratio = 2")
                                       :to-be
                                       4)
                      (cl-weave:expect
                       (btm-config-test-count text "default = true")
                       :to-be
                       1)
                      (cl-weave:expect (btm-config-test-layout-order-p text)
                                       :to-be
                                       t)
                      (cl-weave:expect
                       (or (btm-config-test-contains-p text "[colors]")
                           (btm-config-test-contains-p text "hide_table_gap")
                           (btm-config-test-contains-p text "filter"))
                       :to-be
                       nil))))

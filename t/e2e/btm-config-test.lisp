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
  (let* ((positions
          (mapcar
           (lambda (type)
             (search (format nil "type = ~s" type) text))
           '("cpu" "mem" "net" "disk" "temp" "proc")))
         (first-row (search (format nil "[[row]]~%ratio = 1") text))
         (second-row
          (when first-row
            (search (format nil "[[row]]~%ratio = 1")
                    text
                    :start2 (1+ first-row))))
         (third-row
          (when second-row
            (search (format nil "[[row]]~%ratio = 2")
                    text
                    :start2 (1+ second-row)))))
    (and (every #'integerp positions)
         (loop for previous in positions
               for next in (cdr positions)
               always (< previous next))
         first-row
         second-row
         third-row
         (< first-row second-row third-row))))

(cl-weave:describe "btm configuration projection"
                   (cl-weave:it "expands declarative layout rows"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e:define-btm-layout
                                    ((cpu 2) (mem nil))
                                    ((net 2) (disk 2) (temp nil))
                                    ((:row-ratio 2) (proc nil :default))))
                                 :to-equal
                                 '(quote
                                   (((cpu 2) (mem nil))
                                    ((net 2) (disk 2) (temp nil))
                                    ((:row-ratio 2) (proc nil :default))))))
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
                                                           (concatenate 'string
                                                                        "[flags]~%basic = "
                                                                        "false~%dot_marker = "
                                                                        "false~%rate = ~d")
                                                           rate))
                       :to-be
                       t)
                      (cl-weave:expect
                       (btm-config-test-contains-p text
                                                   (format nil
                                                           (concatenate 'string
                                                                        "[processes]~%case_"
                                                                        "sensitive = false~%"
                                                                        "default_grouped = true")))
                       :to-be
                       t)
                      (cl-weave:expect (btm-config-test-count text "[[row]]")
                                       :to-be
                                       3)
                      (cl-weave:expect
                       (btm-config-test-count text "[[row.child]]")
                       :to-be
                       6)
                      (progn
                        (cl-weave:expect
                         (btm-config-test-count text "ratio = 2")
                         :to-be
                         5)
                        (cl-weave:expect
                         (btm-config-test-count text
                                                (format nil
                                                        "[[row]]~%ratio = 1"))
                         :to-be
                         2)
                        (cl-weave:expect
                         (btm-config-test-count text
                                                (format nil
                                                        "[[row]]~%ratio = 2"))
                         :to-be
                         1)
                        (let* ((ntop.e2e::*btm-layout*
                                (list
                                 (list (list (intern "CPU" :ntop.e2e) nil))))
                               (single-row-text
                                (ntop.e2e:btm-config-string rate)))
                          (cl-weave:expect
                           (btm-config-test-count single-row-text
                                                  (format nil
                                                          "[[row]]~%ratio = 1"))
                           :to-be
                           1)))
                      (cl-weave:expect
                       (btm-config-test-count text "default = true")
                       :to-be
                       1)
                      (cl-weave:expect
                       (btm-config-test-count text (format nil "[[row]]~%ratio = 2"))
                       :to-be
                       1)
                      (cl-weave:expect
                       (btm-config-test-contains-p
                        text
                        (format nil
                                "[[row.child]]~%type = ~s~%ratio = 2"
                                "cpu"))
                       :to-be
                       t)
                      (cl-weave:expect
                       (btm-config-test-contains-p
                        text
                        (format nil
                                "[[row.child]]~%type = ~s~%ratio = 2"
                                "net"))
                       :to-be
                       t)
                      (cl-weave:expect
                       (btm-config-test-contains-p
                        text
                        (format nil
                                "[[row.child]]~%type = ~s~%ratio = 2"
                                "disk"))
                       :to-be
                       t)
                      (cl-weave:expect
                       (btm-config-test-contains-p
                        text
                        (format nil
                                "[[row]]~%ratio = 2~%[[row.child]]~%type = ~s~%default = true"
                                "proc"))
                       :to-be
                       t)
                      (let ((process-child-position
                             (search "type = \"proc\"" text)))
                        (cl-weave:expect
                         (and process-child-position
                              (not (search "ratio = 2"
                                           text
                                           :start2 process-child-position)))
                         :to-be
                         t))
                      (cl-weave:expect (btm-config-test-layout-order-p text)
                                       :to-be
                                       t)
                      (cl-weave:expect
                       (or (btm-config-test-contains-p text "[colors]")
                           (btm-config-test-contains-p text "hide_table_gap")
                           (btm-config-test-contains-p text "filter"))
                       :to-be
                       nil))))

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
            (quote ("cpu" "mem" "temp" "disk" "net" "proc"))))
         (first-row (search (format nil "[[row]]~%ratio = 3") text))
         (second-row
           (when first-row
             (search (format nil "[[row]]~%ratio = 4")
                     text
                     :start2 (1+ first-row))))
         (third-row
           (when second-row
             (search (format nil "[[row]]~%ratio = 3")
                     text
                     :start2 (1+ second-row)))))
    (and (every (function integerp) positions)
         (loop for previous in positions
               for next in (cdr positions)
               always (< previous next))
         first-row
         second-row
         third-row
         (< first-row second-row third-row)
         (search
          (concatenate
           (quote string)
           "[[row.child]]~%ratio = 4~%type = \"mem\"~%[[row.child]]~%"
           "ratio = 3~%[[row.child.child]]~%type = \"temp\"~%"
           "[[row.child.child]]~%type = \"disk\"")
          text))))

(cl-weave:describe
 "btm configuration projection"
 (cl-weave:it
  "expands declarative layout rows"
  (cl-weave:expect
   (macroexpand-1
    '(ntop.e2e:define-btm-layout
      ((cpu nil))
      ((mem 4) (:column 3 ((temp nil) (disk nil))))
      ((net nil) (proc nil :default))))
   :to-equal
   '(quote
     (((cpu nil))
      ((mem 4) (:column 3 ((temp nil) (disk nil))))
      ((net nil) (proc nil :default)))))
 (cl-weave:it-each
  ((:minimum-rate 1) (:configured-rate 700) (:higher-rate 1200))
  "renders deterministic flags and layout at ~A"
  (case-name rate)
  (declare (ignore case-name))
  (let ((text (ntop.e2e:btm-config-string rate)))
    (cl-weave:expect
     (btm-config-test-contains-p
      text
      (format nil
              "[flags]~%basic = false~%dot_marker = false~%rate = ~d"
              rate))
     :to-be t)
    (cl-weave:expect
     (btm-config-test-contains-p
      text
      (format nil
              "[processes]~%case_sensitive = false~%default_grouped = true"))
     :to-be t)
    (cl-weave:expect (btm-config-test-count text "[[row]]") :to-be 3)
    (cl-weave:expect (btm-config-test-count text "[[row.child]]") :to-be 5)
    (cl-weave:expect
     (btm-config-test-count text "[[row.child.child]]")
     :to-be 2)
    (cl-weave:expect
     (btm-config-test-count text "default = true")
     :to-be 1)
    (cl-weave:expect (btm-config-test-layout-order-p text) :to-be t)
    (let* ((ntop.e2e::*btm-layout*
             (list (list (list (intern "CPU" "NTOP.E2E") nil))))
           (single-row-text (ntop.e2e:btm-config-string rate)))
      (cl-weave:expect
       (btm-config-test-contains-p
        single-row-text
        (format nil "[[row]]~%ratio = 1"))
       :to-be t))
    (cl-weave:expect
     (or (btm-config-test-contains-p text "[colors]")
         (btm-config-test-contains-p text "hide_table_gap")
         (btm-config-test-contains-p text "filter"))
     :to-be nil)))))

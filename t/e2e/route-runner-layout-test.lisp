(cl-weave:describe
 "process table wheel input"
 (cl-weave:it
  "emits wheel coordinates inside an empty Processes frame without signaling"
  (let* ((grid (ntop.e2e.test::criterion-test-grid 12 50 ""))
         (route
           (find "Scroll down" (ntop.e2e::route-catalog)
                 :key (function ntop.e2e::route-action)
                 :test (function string=)))
         (subcase (list :context "tables" :gesture "wheel-down"))
         (process-frame
           (find-if
            (lambda (bounds)
              (search "Processes"
                      (ntop.e2e::criterion-row-text grid (first bounds))
                      :test (function char-equal)))
            (ntop.e2e::criterion-projection-frames grid)))
         (inputs nil)
         (caught nil))
    (handler-case
        (setf inputs
              (ntop.e2e::%route-mouse-action-inputs
               route subcase grid 12 50))
      (error (condition)
        (setf caught condition)))
    (cl-weave:expect caught :to-be nil)
    (cl-weave:expect process-frame :to-satisfy (function identity))
    (cl-weave:expect
     (ntop.e2e::%route-mouse-target-unavailable-p subcase grid)
     :to-be t)
    (cl-weave:expect inputs :to-satisfy (function identity))
    (let* ((input (first inputs))
           (prefix (format nil "~c[<65;" #\Escape))
           (first-semicolon (and input (position #\; input)))
           (second-semicolon
             (and first-semicolon
                  (position #\; input :start (1+ first-semicolon))))
           (end (and input (position #\M input :from-end t)))
           (x (and first-semicolon second-semicolon
                   (parse-integer input
                                  :start (1+ first-semicolon)
                                  :end second-semicolon
                                  :junk-allowed t)))
           (y (and second-semicolon end
                   (parse-integer input
                                  :start (1+ second-semicolon)
                                  :end end
                                  :junk-allowed t))))
      (cl-weave:expect
       (and input (search prefix input))
       :to-satisfy (function identity))
      (cl-weave:expect (and (integerp x) (integerp y)) :to-be t)
      (when (and process-frame (integerp x) (integerp y))
        (destructuring-bind (top bottom left right) process-frame
          (cl-weave:expect
           (and (<= (1+ left) x (1+ right))
                (<= (1+ top) y (1+ bottom)))
           :to-be t)))))))

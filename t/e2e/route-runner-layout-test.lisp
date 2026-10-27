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
(cl-weave:describe
 "mouse wheel graph and help geometry"
 (cl-weave:it
  "targets a graph plot and Help frame instead of the dashboard"
  (let* ((graph-grid
           (ntop.e2e.test::criterion-test-hierarchy-grid
            1 4 "38;5;7;49"))
         (help-grid
           (ntop.e2e.test::criterion-test-hierarchy-grid
            1 4 "38;5;7;49"))
         (graph-route
           (find "mouse-70" (ntop.e2e::route-catalog)
                 :key #'ntop.e2e::route-id :test #'string=))
         (help-route
           (find "mouse-69" (ntop.e2e::route-catalog)
                 :key #'ntop.e2e::route-id :test #'string=))
         (graph-input
           (first
            (ntop.e2e::%route-mouse-action-inputs
             graph-route (list :context "graphs" :gesture "wheel-down")
             graph-grid 18 60)))
         (help-input nil)
         (graph-frame
           (find-if
            (lambda (bounds)
              (search "CPU"
                      (ntop.e2e::criterion-row-text graph-grid (first bounds))
                      :test #'char-equal))
            (ntop.e2e::criterion-projection-frames graph-grid)))
         (help-frame nil))
    (ntop.e2e.test::criterion-test-write
     help-grid 2 4 "38;5;7;49" "Help")
    (setf help-input
          (first
           (ntop.e2e::%route-mouse-action-inputs
            help-route (list :context "help" :gesture "wheel-down")
            help-grid 18 60))
          help-frame
          (find-if
           (lambda (bounds)
             (search "Help"
                     (ntop.e2e::criterion-row-text help-grid (first bounds))
                     :test #'char-equal))
           (ntop.e2e::criterion-projection-frames help-grid)))
    (cl-weave:expect graph-frame :to-satisfy (function identity))
    (cl-weave:expect help-frame :to-satisfy (function identity))
    (labels ((coordinates (input)
               (let* ((first-semicolon (position #\; input))
                      (second-semicolon
                        (and first-semicolon
                             (position #\; input :start (1+ first-semicolon))))
                      (end (position #\M input :from-end t)))
                 (values
                  (and first-semicolon second-semicolon
                       (parse-integer input :start (1+ first-semicolon)
                                      :end second-semicolon :junk-allowed t))
                  (and second-semicolon end
                       (parse-integer input :start (1+ second-semicolon)
                                      :end end :junk-allowed t)))))
             (inside-frame-p (input frame)
               (multiple-value-bind (x y) (coordinates input)
                 (and frame x y
                      (destructuring-bind (top bottom left right) frame
                        (and (<= (1+ left) x (1+ right))
                             (<= (1+ top) y (1+ bottom)))))))
             (inside-inner-frame-p (input frame)
               (multiple-value-bind (x y) (coordinates input)
                 (and frame x y
                      (destructuring-bind (top bottom left right) frame
                        (and (< (1+ left) x (1+ right))
                             (< (1+ top) y (1+ bottom)))))))
             (legend-frame ()
               (find-if
                (lambda (bounds)
                  (search "Legend"
                          (ntop.e2e::criterion-row-text
                           graph-grid (first bounds))
                          :test #'char-equal))
                (ntop.e2e::criterion-projection-frames graph-grid))))
      (cl-weave:expect
       (and graph-input (search (format nil "~c[<65;" #\Escape) graph-input))
       :to-satisfy (function identity))
      (cl-weave:expect
       (inside-inner-frame-p graph-input graph-frame)
       :to-be t)
      (cl-weave:expect
       (inside-inner-frame-p help-input help-frame)
       :to-be t)
      (progn
       (cl-weave:expect
        (inside-frame-p graph-input (legend-frame))
        :to-be nil)
       (let* ((table-grid
                (ntop.e2e.test::criterion-test-hierarchy-grid
                 1 4 "38;5;7;49"))
              (table-input nil)
              (table-frame nil))
         (ntop.e2e.test::criterion-test-write
          table-grid 2 4 "38;5;7;49" "Processes")
         (setf table-input
               (first
                (ntop.e2e::%route-mouse-action-inputs
                 help-route (list :context "tables" :gesture "wheel-up")
                 table-grid 18 60))
               table-frame
               (find-if
                (lambda (bounds)
                  (search "Processes"
                          (ntop.e2e::criterion-row-text table-grid (first bounds))
                          :test #'char-equal))
                (ntop.e2e::criterion-projection-frames table-grid)))
         (cl-weave:expect
          (and table-input
               (search (format nil "~c[<64;" #\Escape) table-input))
          :to-satisfy (function identity))
         (cl-weave:expect
          (inside-inner-frame-p table-input table-frame)
          :to-be t))))))
 (cl-weave:it
  "detects CPU graph zoom changes while the adjacent axis stays stable"
  (let ((zoom-in-before (ntop.e2e:make-terminal-grid :rows 2 :cols 40))
        (zoom-in-after (ntop.e2e:make-terminal-grid :rows 2 :cols 40))
        (changed-adjacent (ntop.e2e:make-terminal-grid :rows 2 :cols 40))
        (zoom-out-before (ntop.e2e:make-terminal-grid :rows 2 :cols 40))
        (zoom-out-after (ntop.e2e:make-terminal-grid :rows 2 :cols 40))
        (zoom-in-subcase (list :gesture "wheel-up"))
        (zoom-out-subcase (list :gesture "wheel-down")))
    (ntop.e2e.test::criterion-test-write zoom-in-before 0 0 "0" "CPU 60s 0s 45s")
    (ntop.e2e.test::criterion-test-write zoom-in-after 0 0 "0" "CPU 45s 0s 45s")
    (ntop.e2e.test::criterion-test-write
     changed-adjacent 0 0 "0" "CPU 45s 0s 60s")
    (ntop.e2e.test::criterion-test-write zoom-out-before 0 0 "0" "CPU 60s 0s 60s")
    (ntop.e2e.test::criterion-test-write zoom-out-after 0 0 "0" "CPU 75s 0s 60s")
    (cl-weave:expect
     (ntop.e2e::%route-graph-window-transition-p
      zoom-in-subcase
      zoom-in-before
      zoom-in-after
      60
      45)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-graph-window-transition-p
      zoom-in-subcase
      zoom-in-before
      changed-adjacent
      60
      45)
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-graph-window-transition-p
      zoom-out-subcase
      zoom-out-before
      zoom-out-after
      60
      75)
     :to-be t))))

(in-package #:ntop.e2e.test)

(defun route-frame-capture-test-dashboard-grid
    (cpu-plot cpu-metrics ram-summary network-value network-plot temperature-value
     &key (network-title "Network")
       (temperature-title "Temp")
       (cpu-right-corner #\┐)
       (network-right-corner #\┐)
       (process-title "Processes")
       (process-header "Count Name(n) CPU%(c) Mem%(m)")
       (process-name "optimization")
       (process-count "7")
       (process-cpu "2.9%")
       (process-memory "0.1%")
       (process-row-two "1      ntop         2.4%     0.1%     ")
       (process-row-three "3      sbcl         2.7%     0.3%     ")
       (process-right-corner #\┐))
  (let ((grid (ntop.e2e:make-terminal-grid :rows 12 :cols 40)))
    (labels ((write-cell (row column text)
               (criterion-test-write grid row column "38;5;7;49" text))
             (draw-frame (top bottom left right title right-corner)
               (write-cell top left "┌")
               (loop for column from (1+ left) below right
                     do (write-cell top column "─"))
               (write-cell top right (string right-corner))
               (write-cell top (+ left 2) title)
               (loop for row from (1+ top) below bottom
                     do (write-cell row left "│")
                        (write-cell row right "│"))
               (write-cell bottom left "└")
               (loop for column from (1+ left) below right
                     do (write-cell bottom column "─"))
               (write-cell bottom right "┘")))
      (draw-frame 0 2 0 25 "CPU" cpu-right-corner)
      (draw-frame 0 2 27 39 "Memory" #\┐)
      (draw-frame 3 5 0 14 network-title network-right-corner)
      (draw-frame 3 5 15 30 "Disks" #\┐)
      (draw-frame 3 5 31 39 temperature-title #\┐)
      (draw-frame 6 11 0 39 process-title process-right-corner)
      (write-cell 0 6 cpu-metrics)
      (write-cell 1 1 cpu-plot)
      (write-cell 1 28 (format nil "RAM: ~a" ram-summary))
      (write-cell 4 1 network-value)
      (write-cell 4 5 "│")
      (write-cell 4 6 network-plot)
      (write-cell 4 16 "/dev/disk1s1")
      (write-cell 4 32 temperature-value)
      (write-cell 7 1 process-header)
      (write-cell 8 1 process-count)
      (write-cell 8 8 process-name)
      (write-cell 8 21 process-cpu)
      (write-cell 8 30 process-memory)
      (write-cell 9 1 process-row-two)
      (write-cell 10 1 process-row-three)
      grid)))

(cl-weave:describe
 "route frame stable signature on compact dashboards"
 (cl-weave:it
  "ignores process body reordering while preserving frame structure"
  (flet ((make-grid (&rest options)
           (apply #'route-frame-capture-test-dashboard-grid
                  "⠐⠒"
                  "47.96 35.59 38.37"
                  ""
                  "101."
                  "⣀⣀⣤⣀⣀⣀"
                  "42°C"
                  options)))
    (let* ((initial (make-grid))
           (reordered
             (make-grid :process-row-two
                        "3      sbcl         2.7%     0.3%     "
                        :process-row-three
                        "1      ntop         2.4%     0.1%     "))
           (changed-network-title (make-grid :network-title "Traffic"))
           (changed-temperature-title (make-grid :temperature-title "Heat"))
           (changed-cpu-border (make-grid :cpu-right-corner #\X))
           (changed-network-border
             (make-grid :network-right-corner #\X))
           (changed-process-title (make-grid :process-title "Tasks"))
           (changed-process-header
             (make-grid :process-header "Count  Name(n)  CPU%(c)  Mem%(m)"))
           (changed-process-border
             (make-grid :process-right-corner #\X)))
      (multiple-value-bind (initial-signature initial-canonical)
          (ntop.e2e::%route-grid-stable-signature initial)
        (multiple-value-bind (reordered-signature reordered-canonical)
            (ntop.e2e::%route-grid-stable-signature reordered)
          (cl-weave:expect
           (and
            (char=
             (ntop.e2e:cell-character
              (ntop.e2e:grid-cell-at initial 9 1))
             (ntop.e2e:cell-character
              (ntop.e2e:grid-cell-at reordered 10 1)))
            (char=
             (ntop.e2e:cell-character
              (ntop.e2e:grid-cell-at initial 10 1))
             (ntop.e2e:cell-character
              (ntop.e2e:grid-cell-at reordered 9 1)))
            (not
             (char=
              (ntop.e2e:cell-character
               (ntop.e2e:grid-cell-at initial 9 1))
              (ntop.e2e:cell-character
               (ntop.e2e:grid-cell-at reordered 9 1)))))
           :to-be t)
          (cl-weave:expect initial-signature :to-equal reordered-signature)
          (cl-weave:expect initial-canonical :to-equal reordered-canonical))
        (dolist (variant
                 (list changed-network-title changed-temperature-title
                       changed-cpu-border changed-network-border
                       changed-process-title changed-process-header
                       changed-process-border))
          (multiple-value-bind (signature canonical)
              (ntop.e2e::%route-grid-stable-signature variant)
            (declare (ignore signature))
            (cl-weave:expect
             (not (equal initial-canonical canonical))
             :to-be t))))))))

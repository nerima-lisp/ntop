(in-package #:ntop.e2e.test)

(defun route-frame-capture-test-dashboard-grid
    (cpu-plot cpu-metrics ram-summary network-value network-plot temperature-value
              &key (rows 12) (columns 40) (cpu-bottom 2) (cpu-right 25)
              (cpu-title "CPU") cpu-graph wide-dashboard
              (memory-title "Memory") memory-graph
              (ram-label "RAM:") (swap-label "SWP:")
              (swap-summary "96% 17.3GiB/18.0GiB")
              (network-title "Network")
              network-graph
              network-graph-cells
              (network-rx-label "RX:")
              (network-rx-value "25.6Mb/s")
              (network-rx-total "94.0GB")
              (network-tx-label "TX:")
              (network-tx-value "1.7Mb/s")
              (network-tx-total "18.6GB")
              (network-all-label "All:")
              (disk-title "Disks")
              (disk-heading "Free(n)")
              (disk-free-value "978GB")
              (temperature-title "Temp")
              (temperature-panel-title "Temperatures")
              (temperature-heading "Temp(t)")
              (temperature-label "PMU tdev1 (1)")
              (cpu-right-corner #\┐)
              (memory-right-corner #\┐)
              (network-right-corner #\┐)
              (network-inset-border-character #\─)
              (disk-right-corner #\┐)
              (temperature-right-corner #\┐)
              (process-title "Processes")
              (process-header "Count Name(n) CPU%(c) Mem%(m)")
              (process-name "optimization")
              (process-count "7")
              (process-cpu "2.9%")
              (process-memory "0.1%")
              (process-row-two "1      ntop         2.4%     0.1%     ")
              (process-row-three "3      sbcl         2.7%     0.3%     ")
              (process-right-corner #\┐))
  (let ((grid (ntop.e2e:make-terminal-grid :rows rows :cols columns)))
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
      (draw-frame 0 cpu-bottom 0 cpu-right cpu-title cpu-right-corner)
      (unless wide-dashboard
        (draw-frame 0 2 27 39 memory-title memory-right-corner)
        (draw-frame 3 5 0 14 network-title network-right-corner)
        (draw-frame 3 5 15 30 disk-title disk-right-corner)
        (draw-frame 3 5 31 39 temperature-title temperature-right-corner)
        (draw-frame 6 11 0 39 process-title process-right-corner))
      (when wide-dashboard
        (draw-frame 0 14 147 219 memory-title memory-right-corner)
        (draw-frame 1 5 189 218 "" #\┐)
        (draw-frame 15 29 0 87 network-title network-right-corner)
        (draw-frame 16 19 59 87 "" #\┐)
        (write-cell 19 70 (string network-inset-border-character))
        (draw-frame 15 29 88 175 disk-title disk-right-corner)
        (draw-frame 15 29 176 219 temperature-panel-title temperature-right-corner)
        (write-cell 2 190 (format nil "~a ~a" ram-label ram-summary))
        (write-cell 3 190 (format nil "~a ~a" swap-label swap-summary))
        (write-cell 16 2 network-value)
        (progn
          (write-cell 27 5 "└")
          (loop for column from 6 below 88
                do (write-cell 27 column "─"))
          (write-cell 28 4 "60s")
          (write-cell 28 85 "0s"))
        (write-cell 17 60 network-rx-label)
        (write-cell 17 64 network-rx-value)
        (write-cell 17 74 network-all-label)
        (write-cell 17 79 network-rx-total)
        (write-cell 18 60 network-tx-label)
        (write-cell 18 64 network-tx-value)
        (write-cell 18 74 network-all-label)
        (write-cell 18 79 network-tx-total)
        (when network-graph
          (write-cell 20 57 network-graph))
        (dolist (cell network-graph-cells)
          (destructuring-bind (row column text) cell
            (write-cell row column text)))
        (progn
          (write-cell 16 89 "Disk(d)")
          (write-cell 16 103 "Mount(m)")
          (write-cell 16 135 disk-heading))
        (write-cell 20 135 disk-free-value)
        (progn
          (write-cell 16 177 "Sensor(s)")
          (write-cell 16 205 temperature-heading))
        (write-cell 20 180 temperature-label)
        (write-cell 20 205 temperature-value))
      (write-cell 0 6 cpu-metrics)
      (write-cell 1 1 cpu-plot)
      (when cpu-graph
        (write-cell 1 58 cpu-graph))
      (when memory-graph
        (write-cell 10 217 memory-graph))
      (unless wide-dashboard
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
        (write-cell 10 1 process-row-three))
      grid)))

(cl-weave:describe
 "route frame stable signature on compact dashboards"
 (cl-weave:it
  "masks changing panel interiors while preserving layout structure"
  (labels ((make-grid (cpu-plot &rest options)
           (let* ((ram-summary (getf options :ram-summary ""))
                  (network-value (getf options :network-value "101."))
                  (network-plot (getf options :network-plot "⣀⣀⣤⣀⣀⣀"))
                  (temperature-value (getf options :temperature-value "42°C")))
             (remf options :ram-summary)
             (remf options :network-value)
             (remf options :network-plot)
             (remf options :temperature-value)
             (apply (function route-frame-capture-test-dashboard-grid)
                    cpu-plot
                    "47.96 35.59 38.37"
                    ram-summary
                    network-value
                    network-plot
                    temperature-value
                    options)))
         (make-divider-grid (interior)
           (let ((grid (make-grid "⠐⠒")))
             (criterion-test-write grid 9 0 "38;5;7;49" "├")
             (criterion-test-write grid 9 39 "38;5;7;49" "┤")
             (criterion-test-write grid 9 15 "38;5;7;49" interior)
             grid)))
    (let* ((initial (make-grid "⠐⠒"))
           (reordered
             (make-grid "⠐⠒"
                        :process-row-two "3      sbcl         2.7%     0.3%     "
                        :process-row-three "1      ntop         2.4%     0.1%     "))
           (wide-initial
             (make-grid "⠐⠒" :rows 30 :columns 220 :wide-dashboard t))
           (wide-dynamic
             (make-grid "⣿⣤" :rows 30 :columns 220 :wide-dashboard t
                        :ram-summary "25% 4.0GiB/16.0GiB"
                        :swap-summary "5% 1.0GiB/20.0GiB"
                        :network-value "499Mb/s"
                        :network-rx-value "90.0Mb/s"
                        :network-tx-value "30.0Mb/s"
                        :network-graph-cells
                        (list (list 20 57 "⣤") (list 22 10 "⣀"))
                        :memory-graph "⣿"
                        :disk-free-value "100GB"
                        :temperature-value "99°C"))
           (wide-outside-change
             (make-grid "⠐⠒" :rows 30 :columns 220 :wide-dashboard t
                        :cpu-graph "x"))
           (divider-initial (make-divider-grid "X"))
           (divider-changed (make-divider-grid "Y"))
           (structural-changes
             (list (make-grid "⠐⠒" :network-title "Traffic")
                   (make-grid "⠐⠒" :temperature-title "Heat")
                   (make-grid "⠐⠒" :cpu-right-corner #\X)
                   (make-grid "⠐⠒" :network-right-corner #\X)
                   (make-grid "⠐⠒" :disk-right-corner #\X)
                   (make-grid "⠐⠒" :temperature-right-corner #\X)
                   (make-grid "⠐⠒" :process-title "Tasks")
                   (make-grid "⠐⠒"
                              :process-header "Count  Name(n)  CPU%(c)  Mem%(m)")
                   (make-grid "⠐⠒" :process-right-corner #\X)
                   (make-grid "⠐⠒" :rows 30 :columns 220 :wide-dashboard t
                              :network-inset-border-character #\X))))
      (multiple-value-bind (initial-signature initial-canonical)
          (ntop.e2e::%route-grid-stable-signature initial)
        (multiple-value-bind (reordered-signature reordered-canonical)
            (ntop.e2e::%route-grid-stable-signature reordered)
          (cl-weave:expect (equal initial-signature reordered-signature)
                           :to-be t)
          (cl-weave:expect (equal initial-canonical reordered-canonical)
                           :to-be t))
        (dolist (variant structural-changes)
          (cl-weave:expect
           (not (equal initial-canonical
                       (nth-value 1
                                  (ntop.e2e::%route-grid-stable-signature variant))))
           :to-be t)))
      (multiple-value-bind (wide-signature wide-canonical)
          (ntop.e2e::%route-grid-stable-signature wide-initial)
        (multiple-value-bind (dynamic-signature dynamic-canonical)
            (ntop.e2e::%route-grid-stable-signature wide-dynamic)
          (cl-weave:expect (equal wide-signature dynamic-signature) :to-be t)
          (cl-weave:expect (equal wide-canonical dynamic-canonical) :to-be t))
        (cl-weave:expect
         (not (equal wide-canonical
                     (nth-value 1
                                (ntop.e2e::%route-grid-stable-signature
                                 wide-outside-change))))
         :to-be t)
        (dolist (heading-change
                (list (make-grid "⠐⠒" :rows 30 :columns 220 :wide-dashboard t
                                 :disk-heading "Free(q)")
                      (make-grid "⠐⠒" :rows 30 :columns 220 :wide-dashboard t
                                 :temperature-heading "Heat(t)")))
          (multiple-value-bind (heading-signature heading-canonical)
              (ntop.e2e::%route-grid-stable-signature heading-change)
            (cl-weave:expect (not (equal wide-signature heading-signature))
                             :to-be t)
            (cl-weave:expect (not (equal wide-canonical heading-canonical))
                             :to-be t))))
      (cl-weave:expect
       (not (equal (nth-value 1
                             (ntop.e2e::%route-grid-stable-signature
                              divider-initial))
                   (nth-value 1
                              (ntop.e2e::%route-grid-stable-signature
                               divider-changed))))
       :to-be t)))))

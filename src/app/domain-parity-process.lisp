(in-package #:ntop.app.domain)

(defparameter *btm-parity-column-data*
  (define-parity-columns
   ("CPU%(c)" "CpuPercent"
              ("cpu%")
              "all"
              "yes"
              "3 CPU"
              ("src/widgets/process_table/process_columns.rs:17-230"
               "src/widgets/process_table.rs:301-381"))
   ("Mem(m)" "MemValue"
             ("mem" "mem%" "memory" "memory%")
             "all"
             "when process_memory_as_value"
             "4 Mem"
             ("src/widgets/process_table/process_columns.rs:17-230"
              "src/widgets/process_table.rs:301-381"))
   ("Mem%(m)" "MemPercent"
              ("mem" "mem%" "memory" "memory%")
              "all"
              "unless process_memory_as_value"
              "4 Mem"
              ("src/widgets/process_table/process_columns.rs:17-230"
               "src/widgets/process_table.rs:301-381"))
   ("Virt" "VirtualMem"
           ("virt" "virtual" "virtmem" "virtual memory")
           "all"
           "no; optional"
           "optional"
           ("src/widgets/process_table/process_columns.rs:17-230"))
   ("PID(p)" "Pid"
             ("pid")
             "all"
             "when not grouped"
             "1 PID/Count"
             ("src/widgets/process_table/process_columns.rs:17-230"
              "src/widgets/process_table.rs:301-381"))
   ("Count" "Count"
            ("count")
            "all"
            "when grouped"
            "1 PID/Count"
            ("src/widgets/process_table/process_columns.rs:17-230"
             "src/widgets/process_table.rs:301-381"))
   ("Name(n)" "Name"
              ("name")
              "all"
              "unless process_command"
              "2 Name/Command"
              ("src/widgets/process_table/process_columns.rs:17-230"
               "src/widgets/process_table.rs:301-381"))
   ("Command(n)" "Command"
                 ("command")
                 "all"
                 "when process_command"
                 "2 Name/Command"
                 ("src/widgets/process_table/process_columns.rs:17-230"
                  "src/widgets/process_table.rs:301-381"))
   ("R/s" "ReadPerSecond"
          ("read" "r/s" "rps")
          "all"
          "yes"
          "5 R/s"
          ("src/widgets/process_table/process_columns.rs:17-230"
           "src/widgets/process_table.rs:301-381"))
   ("W/s" "WritePerSecond"
          ("write" "w/s" "wps")
          "all"
          "yes"
          "6 W/s"
          ("src/widgets/process_table/process_columns.rs:17-230"
           "src/widgets/process_table.rs:301-381"))
   ("T.Read" "TotalRead"
             ("tread" "t.read" "total read")
             "all"
             "yes"
             "7 T.Read"
             ("src/widgets/process_table/process_columns.rs:17-230"
              "src/widgets/process_table.rs:301-381"))
   ("T.Write" "TotalWrite"
              ("twrite" "t.write" "total write")
              "all"
              "yes"
              "8 T.Write"
              ("src/widgets/process_table/process_columns.rs:17-230"
               "src/widgets/process_table.rs:301-381"))
   ("State" "State"
            ("state")
            "all"
            "yes at startup; may hide after grouping"
            "10 State"
            ("src/widgets/process_table/process_columns.rs:17-230"
             "src/widgets/process_table.rs:301-381"))
   ("User" "User"
           ("user")
           "all"
           "yes at startup; may hide after grouping"
           "9 User"
           ("src/widgets/process_table/process_columns.rs:17-230"
            "src/widgets/process_table.rs:301-381"))
   ("Time" "Time"
           ("time")
           "all"
           "yes"
           "11 Time"
           ("src/widgets/process_table/process_columns.rs:17-230"
            "src/widgets/process_table.rs:301-381"))
   ("Nice" "Nice"
           ("nice")
           "Unix"
           "no; optional"
           "optional"
           ("src/widgets/process_table/process_columns.rs:17-230"))
   ("Priority" "Priority"
               ("priority")
               "all"
               "yes"
               "12 Priority"
               ("src/widgets/process_table/process_columns.rs:17-230"
                "src/widgets/process_table.rs:301-381"))
   ("GMem" "GpuMemValue"
           ("gmem" "gmem%")
           "gpu feature"
           "optional; value mode when configured"
           "GPU memory"
           ("src/widgets/process_table/process_columns.rs:17-230"))
   ("GMem%" "GpuMemPercent"
            ("gmem" "gmem%")
            "gpu feature"
            "optional; percentage mode when configured"
            "GPU memory"
            ("src/widgets/process_table/process_columns.rs:17-230"))
   ("GPU%" "GpuUtilPercent"
           ("gpu%")
           "gpu feature"
           "no; optional"
           "optional"
           ("src/widgets/process_table/process_columns.rs:17-230"))))

(defparameter *btm-parity-search-field-data*
  (define-parity-search-fields
   ("PID" ("pid") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("CPU%" ("cpu" "cpu%") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Memory bytes" ("memb") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Memory%" ("mem" "mem%") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Read/s" ("read" "r/s" "rps") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Write/s" ("write" "w/s" "wps") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Total read" ("tread" "t.read") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Total write" ("twrite" "t.write") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Process name or command" () "all" "yes" ("src/widgets/process_table/query.rs:181-257"))
   ("State" ("state") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("User" ("user") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Time" ("time") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Nice" ("nice") "Unix" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("Priority" ("priority") "all" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("GPU%" ("gpu%") "gpu feature" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("GPU memory bytes" ("gmem") "gpu feature" "no" ("src/widgets/process_table/query.rs:181-257"))
   ("GPU memory%" ("gmem%") "gpu feature" "no" ("src/widgets/process_table/query.rs:181-257"))))

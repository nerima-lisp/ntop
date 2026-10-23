(in-package #:ntop.app.domain)

(defparameter *btm-parity-flag-data*
  (define-parity-flags
   ("autohide_time" "" () "boolean" () "all platforms" ("src/options/args.rs:95"))
   ("basic" "-b" () "boolean" () "all platforms" ("src/options/args.rs:105"))
   ("config_location" "-C" ("--config") "path" () "all platforms" ("src/options/args.rs:114-126"))
   ("default_time_value" "-t" () "duration" () "all platforms" ("src/options/args.rs:128"))
   ("default_widget_count" ""
                           ()
                           "integer"
                           ()
                           "requires default_widget_type"
                           ("src/options/args.rs:140"))
   ("default_widget_type" ""
                          ()
                          "enum"
                          ("cpu" "mem"
                                 "net"
                                 "network"
                                 "proc"
                                 "process"
                                 "processes"
                                 "temp"
                                 "temperature"
                                 "disk"
                                 "batt"
                                 "battery")
                          "batt/battery values require battery feature"
                          ("src/options/args.rs:164-201"))
   ("disable_click" "" () "boolean" () "all platforms" ("src/options/args.rs:202"))
   ("disable_keys" "" () "boolean" () "all platforms" ("src/options/args.rs:211"))
   ("dot_marker" "-m" () "boolean" () "all platforms" ("src/options/args.rs:221"))
   ("expanded" "-e" () "boolean" () "ignored in basic mode" ("src/options/args.rs:231"))
   ("hide_time" "" () "boolean" () "all platforms" ("src/options/args.rs:240"))
   ("rate" "-r" () "duration" () "all platforms" ("src/options/args.rs:243"))
   ("retention" "" () "duration" () "all platforms" ("src/options/args.rs:254"))
   ("show_table_scroll_position" "" () "boolean" () "all platforms" ("src/options/args.rs:264"))
   ("time_delta" "-d" () "duration" () "all platforms" ("src/options/args.rs:272"))
   ("case_sensitive" "-S" () "boolean" () "all platforms" ("src/options/args.rs:289"))
   ("current_usage" "-u" () "boolean" () "all platforms" ("src/options/args.rs:300"))
   ("disable_advanced_kill" "" () "boolean" () "Unix-like systems" ("src/options/args.rs:310"))
   ("read_only" "" () "boolean" () "all platforms" ("src/options/args.rs:320"))
   ("hide_k_threads" "" () "boolean" () "Linux only" ("src/options/args.rs:331"))
   ("get_threads" "" () "boolean" () "all platforms" ("src/options/args.rs:339"))
   ("process_default_sort" "" () "column-name" () "all platforms" ("src/options/args.rs:347"))
   ("group_processes" "-g" () "boolean" () "ignored in tree mode" ("src/options/args.rs:358"))
   ("process_memory_as_value" "" () "boolean" () "all platforms" ("src/options/args.rs:369"))
   ("process_command" "" () "boolean" () "all platforms" ("src/options/args.rs:378"))
   ("regex" "-R" () "boolean" () "all platforms" ("src/options/args.rs:386"))
   ("tree" "-T" () "boolean" () "all platforms" ("src/options/args.rs:389"))
   ("tree_collapse" "" () "boolean" () "all platforms" ("src/options/args.rs:397"))
   ("unnormalized_cpu" "-n" () "boolean" () "all platforms" ("src/options/args.rs:405"))
   ("whole_word" "-W" () "boolean" () "all platforms" ("src/options/args.rs:414"))
   ("celsius" "-c"
              ()
              "boolean"
              ()
              "mutually exclusive temperature unit"
              ("src/options/args.rs:429"))
   ("fahrenheit" "-f"
                 ()
                 "boolean"
                 ()
                 "mutually exclusive temperature unit"
                 ("src/options/args.rs:439"))
   ("kelvin" "-k" () "boolean" () "mutually exclusive temperature unit" ("src/options/args.rs:448"))
   ("cpu_left_legend" "-l" () "boolean" () "all platforms" ("src/options/args.rs:485"))
   ("default_cpu_entry" ""
                        ()
                        "enum"
                        ("all" "avg" "average")
                        "all platforms"
                        ("src/options/args.rs:494"))
   ("hide_avg_cpu" "-a" () "boolean" () "all platforms" ("src/options/args.rs:503"))
   ("memory_legend" ""
                    ()
                    "enum"
                    ("none" "top-left"
                            "top"
                            "top-right"
                            "left"
                            "right"
                            "bottom-left"
                            "bottom"
                            "bottom-right")
                    "all platforms"
                    ("src/options/args.rs:517"))
   ("enable_cache_memory" "" () "boolean" () "not Windows" ("src/options/args.rs:528"))
   ("free_arc" "" () "boolean" () "all platforms" ("src/options/args.rs:537"))
   ("short_gpu_names" "" () "boolean" () "all platforms" ("src/options/args.rs:546"))
   ("network_legend" ""
                     ()
                     "enum"
                     ("none" "top-left"
                             "top"
                             "top-right"
                             "left"
                             "right"
                             "bottom-left"
                             "bottom"
                             "bottom-right")
                     "all platforms"
                     ("src/options/args.rs:559"))
   ("network_use_bytes" "" () "boolean" () "all platforms" ("src/options/args.rs:570"))
   ("network_use_binary_prefix" "" () "boolean" () "all platforms" ("src/options/args.rs:579"))
   ("network_use_log" "" () "boolean" () "all platforms" ("src/options/args.rs:589"))
   ("use_old_network_legend" "" () "boolean" () "deprecated; accepted" ("src/options/args.rs:598"))
   ("show_packets" "" () "boolean" () "all platforms" ("src/options/args.rs:607"))
   ("network_start_zeroed" "" () "boolean" () "all platforms" ("src/options/args.rs:616"))
   ("battery" ""
              ()
              "boolean"
              ()
              "battery feature; ignored in custom layouts"
              ("src/options/args.rs:631"))
   ("disable_gpu" "" () "boolean" () "gpu feature" ("src/options/args.rs:647"))
   ("theme" ""
            ()
            "enum"
            ("default" "default-light" "gruvbox" "gruvbox-light" "nord" "nord-light")
            "all platforms"
            ("src/options/args.rs:655"))
   ("help" "-h" () "control" () "parser control" ("src/options/args.rs:688"))
   ("version" "-V" () "control" () "parser control" ("src/options/args.rs:691"))))

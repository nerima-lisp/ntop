(in-package #:ntop.app.domain)

(define-parity-table *btm-parity-mouse-data* :mouse
   ("Scroll up" "wheel-up"
    ("graphs" "tables" "help" "kill dialog")
    "Zooms graphs in; scrolls tables or help up; moves the kill list up."
    "--disable_click disables mouse capture"
    ("src/event.rs:31-34" "src/app.rs:1930-1940" "src/lib.rs:363-369"))
   ("Scroll down" "wheel-down"
    ("graphs" "tables" "help" "kill dialog")
    "Zooms graphs out; scrolls tables or help down; moves the kill list down."
    "--disable_click disables mouse capture"
    ("src/event.rs:31-34" "src/app.rs:1942-1952" "src/lib.rs:363-369"))
   ("Focus widget" "left-button-down"
    ("all widgets")
    "Selects the widget under the pointer."
    "--disable_click disables mouse capture"
    ("src/app.rs:2048-2290" "src/lib.rs:363-369"))
   ("Move basic-mode widget" "left-button-down"
    ("basic-mode arrows")
    "Arrow buttons change the selected basic-mode widget."
    "--disable_click disables mouse capture"
    ("src/app.rs:2048-2290" "src/lib.rs:363-369"))
   ("Select process row" "left-button-down"
    ("process table")
    "Selects a row; clicking it again in tree mode toggles its branch."
    "--disable_click disables mouse capture"
    ("src/app.rs:2048-2290" "src/lib.rs:363-369"))
   ("Select process sort" "left-button-down"
    ("process sort list")
    "Selects the clicked sort choice."
    "--disable_click disables mouse capture"
    ("src/app.rs:2048-2290" "src/lib.rs:363-369"))
   ("Select CPU series" "left-button-down"
    ("CPU legend")
    "Selects the clicked CPU series."
    "--disable_click disables mouse capture"
    ("src/app.rs:2048-2290" "src/lib.rs:363-369"))
   ("Select table row or column" "left-button-down"
    ("temperature table" "disk table")
    "Selects rows or sorts by a clicked header."
    "--disable_click disables mouse capture"
    ("src/app.rs:2048-2290" "src/lib.rs:363-369"))
   ("Select battery" "left-button-down"
    ("battery widget")
    "Selects the clicked battery tab."
    "--disable_click disables mouse capture"
    ("src/app.rs:2048-2290" "src/lib.rs:363-369"))
   ("Choose kill action" "left-button-down"
    ("kill dialog")
    "Invokes a clicked dialog choice."
    "--disable_click disables mouse capture"
    ("src/app.rs:2117-2121"
     "src/canvas/dialogs/process_kill_dialog.rs:388-421"
     "src/lib.rs:363-369"))
   ("Ignore other buttons" "right/middle/other button"
    ("all widgets")
    "Right, middle, and other button-down events do nothing."
    "--disable_click disables mouse capture"
    ("src/event.rs:35-48" "src/lib.rs:363-369"))
   ("Ignore pointer motion" "move/drag/release"
    ("all widgets")
    "Pointer motion, drag, and release events are ignored."
    "--disable_click disables mouse capture"
    ("src/event.rs:35-48" "src/lib.rs:363-369")))

(define-parity-table *btm-parity-extension-data* :extension
   ("Cycle selected widget view" "v"
                                 ("Memory" "Temperatures" "Disks")
                                 "Memory to GPU; Temps to graph to Battery; Disks to I/O graph."
                                 "Spec addition absent from btm keys."
                                 ("spec: U8" "src/app.rs:1022-1253"))
   ("Scroll history backward" "["
                              ("graphs")
                              "Moves the displayed history back one zoom-width."
                              "Spec addition absent from btm keys."
                              ("spec: U8" "src/app.rs:1022-1253"))
   ("Scroll history forward" "]"
                             ("graphs")
                             "Moves the displayed history forward one zoom-width."
                             "Spec addition absent from btm keys."
                             ("spec: U8" "src/app.rs:1022-1253"))
   ("Return graph to current" "G"
                              ("graphs")
                              "Returns the graph to the current time."
                              "Graph-scoped extension; btm G otherwise goes to the last row."
                              ("spec: U8" "src/app.rs:1022-1253")))

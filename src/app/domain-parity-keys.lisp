(in-package #:ntop.app.domain)

(define-parity-table *btm-parity-key-data* :key
   ("Quit" ("q" "Q")
           ("normal")
           "Exits unless process or help search is active."
           ("src/event.rs:58-60"))
   ("Quit" ("Ctrl+C") ("all") "Always exits, including during search." ("src/event.rs:95-98"))
   ("Quit" ("Shift+Q") ("normal") "Exits unless search is active." ("src/event.rs:125-132"))
   ("Open help" ("?") ("normal") "Shows help." ("src/app.rs:1169-1171"))
   ("Freeze updates" ("f") ("normal") "Toggles collection freeze." ("src/app.rs:1073-1075"))
   ("Expand widget" ("e")
                    ("normal")
                    "Expands/restores unless basic or process search."
                    ("src/app.rs:1195-1198; src/app.rs:1264-1285"))
   ("Reset collection" ("Ctrl+R") ("normal") "Resets collected data." ("src/event.rs:106-108"))
   ("Move widget selection left" ("Ctrl+Left" "Shift+Left" "H" "A")
                                 ("normal")
                                 "Moves selection left."
                                 ("src/event.rs:102-130" "src/app.rs:1173-1176"))
   ("Move widget selection right" ("Ctrl+Right" "Shift+Right" "L" "D")
                                  ("normal")
                                  "Moves selection right."
                                  ("src/event.rs:102-130" "src/app.rs:1173-1176"))
   ("Move widget selection up" ("Ctrl+Up" "Shift+Up" "K" "W")
                               ("normal")
                               "Moves selection up."
                               ("src/event.rs:102-130" "src/app.rs:1173-1176"))
   ("Move widget selection down" ("Ctrl+Down" "Shift+Down" "J" "S")
                                 ("normal")
                                 "Moves selection down."
                                 ("src/event.rs:102-130" "src/app.rs:1173-1176"))
   ("Go to first row" ("Home" "gg")
                      ("tables" "help")
                      "Moves to the first row or top of help."
                      ("src/event.rs:61-62; src/app.rs:1055-1070; src/app.rs:1707-1763"))
   ("Go to last row" ("End" "G")
                     ("tables" "help")
                     "Moves to the last row or bottom of help."
                     ("src/event.rs:61-62; src/app.rs:1072; src/app.rs:1766-1823"))
   ("Move to previous row" ("Up" "k")
                           ("tables" "help")
                           "Moves one row up."
                           ("src/event.rs:63-64; src/app.rs:588-609"))
   ("Move to next row" ("Down" "j")
                       ("tables" "help")
                       "Moves one row down."
                       ("src/event.rs:63-64; src/app.rs:588-609"))
   ("Page up" ("PageUp")
              ("tables" "help" "kill dialog")
              "Scrolls up by a page."
              ("src/event.rs:80-81; src/app.rs:733-751"))
   ("Page down" ("PageDown")
                ("tables" "help" "kill dialog")
                "Scrolls down by a page."
                ("src/event.rs:80-81; src/app.rs:753-770"))
   ("Half-page up" ("Ctrl+U")
                   ("tables" "help")
                   "Scrolls up by half a page outside search."
                   ("src/event.rs:115; src/app.rs:775-790"))
   ("Half-page down" ("Ctrl+D")
                     ("tables" "help")
                     "Scrolls down by half a page."
                     ("src/event.rs:114; src/app.rs:793-810"))
   ("Move left within widget" ("Left" "Alt+h")
                              ("process tree" "search cursor" "battery tabs" "kill dialog")
                              "Moves the tree row, search cursor, battery tab, or dialog choice."
                              ("src/event.rs:65-94" "src/app.rs:612-716"))
   ("Move right within widget" ("Right" "Alt+l")
                               ("process tree" "search cursor" "battery tabs" "kill dialog")
                               "Moves the tree row, search cursor, battery tab, or dialog choice."
                               ("src/event.rs:65-94" "src/app.rs:612-716"))
   ("Open process search" ("/" "Ctrl+F")
                          ("process table" "process sort")
                          "Opens process search."
                          ("src/event.rs:101; src/app.rs:333-351"))
   ("Open help search" ("/" "Ctrl+F")
                       ("help")
                       "Opens help search."
                       ("src/app.rs:352-356; src/app.rs:962-965"))
   ("Enter search text" ("printable characters")
                        ("process search" "help search")
                        "Inserts printable text."
                        ("src/app.rs:900-968"))
   ("Toggle search case" ("F1" "Alt+c" "Alt+C")
                         ("process search" "help search")
                         "Toggles case sensitivity."
                         ("src/event.rs:74-75; src/event.rs:86-89"))
   ("Toggle whole-word search" ("F2" "Alt+w" "Alt+W")
                               ("process search" "help search")
                               "Toggles whole-word matching."
                               ("src/event.rs:75-76; src/event.rs:88-90"))
   ("Toggle regex search" ("F3" "Alt+r" "Alt+R")
                          ("process search" "help search")
                          "Toggles regex matching."
                          ("src/event.rs:76-77; src/event.rs:89-91"))
   ("Move search cursor to start" ("Ctrl+A")
                                  ("process search" "help search")
                                  "Moves the cursor to the start."
                                  ("src/event.rs:109; src/app.rs:812-833"))
   ("Move search cursor to end" ("Ctrl+E")
                                ("process search" "help search")
                                "Moves the cursor to the end."
                                ("src/event.rs:110; src/app.rs:835-857"))
   ("Clear search" ("Ctrl+U")
                   ("process search" "help search")
                   "Clears the query and leaves search open."
                   ("src/event.rs:111; src/app.rs:865-898"))
   ("Delete previous word" ("Ctrl+W")
                           ("process search" "help search")
                           "Deletes the previous word."
                           ("src/event.rs:112; src/app.rs:561-585"))
   ("Delete previous character" ("Backspace" "Ctrl+H")
                                ("process search" "help search")
                                "Deletes the character before the cursor."
                                ("src/event.rs:72-73; src/event.rs:113; src/app.rs:561-585"))
   ("Delete next character" ("Delete")
                            ("process search" "help search")
                            "Deletes at the cursor."
                            ("src/event.rs:73; src/app.rs:528-553"))
   ("Delete selected process" ("Delete")
                              ("process table")
                              "Opens the process kill dialog."
                              ("src/event.rs:73; src/app.rs:528-557"))
   ("Accept current choice" ("Enter")
                            ("process search" "help search" "process sort" "kill dialog")
                            "Closes search, applies sorting, or confirms a kill choice."
                            ("src/event.rs:70; src/app.rs:490-525"))
   ("Close current modal" ("Escape")
                          ("process search" "help search"
                                            "process sort"
                                            "kill dialog"
                                            "expanded widget")
                          "Closes a modal or restores an expanded widget."
                          ("src/event.rs:69; src/app.rs:228-292"))
   ("Toggle process table tab" ("Tab")
                               ("process table")
                               "Toggles the process tab."
                               ("src/event.rs:71; src/app.rs:319-331"))
   ("Sort process by CPU" ("c")
                          ("process table")
                          "Selects CPU% as the sort column."
                          ("src/app.rs:1076-1085"))
   ("Sort process by memory" ("m")
                             ("process table")
                             "Selects memory as the sort column."
                             ("src/app.rs:1086-1102"))
   ("Sort process by PID or count" ("p")
                                   ("process table")
                                   "Selects PID or grouped count as the sort column."
                                   ("src/app.rs:1103-1119"))
   ("Sort process by name" ("n")
                           ("process table")
                           "Selects process name or command as the sort column."
                           ("src/app.rs:1130-1146"))
   ("Sort process by GPU utilization" ("C")
                                      ("process table")
                                      "Selects GPU% when built with GPU support."
                                      ("src/app.rs:1158-1167"))
   ("Sort process by GPU memory" ("M")
                                 ("process table")
                                 "Selects GPU memory when built with GPU support."
                                 ("src/app.rs:1147-1156"))
   ("Toggle process command display" ("P")
                                     ("process table")
                                     "Toggles command display."
                                     ("src/app.rs:1120-1129"))
   ("Open process sort list" ("s" "F6")
                             ("process table")
                             "Opens or closes process sorting."
                             ("src/event.rs:78-79; src/app.rs:1199-1211"))
   ("Reverse sort order" ("I") ("process table") "Reverses the active sort." ("src/app.rs:1239"))
   ("Toggle process tree" ("t" "F5")
                          ("process table")
                          "Toggles process tree mode."
                          ("src/event.rs:77; src/app.rs:1177-1180"))
   ("Open process kill dialog" ("dd" "F9")
                               ("process table")
                               "Opens the process kill dialog; dd is a sequence."
                               ("src/event.rs:79; src/app.rs:1027-1047"))
   ("Toggle process tree branch" ("Space")
                                 ("process tree")
                                 "Toggles the selected tree branch."
                                 ("src/event.rs:67; src/app.rs:718-731"))
   ("Toggle process tree branch" ("+" "-")
                                 ("process tree")
                                 "Toggles the selected tree branch."
                                 ("src/app.rs:1195-1197; src/app.rs:1954-1980"))
   ("Zoom graph in" ("+") ("graphs") "Zooms in." ("src/app.rs:1195-1197; src/app.rs:1954-1960"))
   ("Zoom graph out" ("-") ("graphs") "Zooms out." ("src/app.rs:1195-1197; src/app.rs:1963-1969"))
   ("Reset graph zoom" ("=")
                       ("graphs")
                       "Resets graph zoom."
                       ("src/app.rs:1195-1197; src/app.rs:2042-2045"))
   ("Toggle memory values" ("%")
                           ("process table")
                           "Toggles memory values/percentages."
                           ("src/app.rs:1240"))
   ("Toggle kernel threads" ("z")
                            ("process table")
                            "Toggles kernel-thread visibility on Linux."
                            ("src/app.rs:1241-1251"))
   ("Select disk column 0" ("d")
                           ("disk table")
                           "Selects the first disk sort column."
                           ("src/app.rs:1027-1053"))
   ("Select disk column 1" ("m")
                           ("disk table")
                           "Selects the second disk sort column."
                           ("src/app.rs:1086-1102"))
   ("Select disk column 2" ("u")
                           ("disk table")
                           "Selects the third disk sort column."
                           ("src/app.rs:1212-1219"))
   ("Select disk column 3" ("n")
                           ("disk table")
                           "Selects the fourth disk sort column."
                           ("src/app.rs:1130-1146"))
   ("Select disk column 4" ("t")
                           ("disk table")
                           "Selects the fifth disk sort column."
                           ("src/app.rs:1177-1194"))
   ("Select disk column 5" ("p")
                           ("disk table")
                           "Selects the sixth disk sort column."
                           ("src/app.rs:1103-1119"))
   ("Select disk column 6" ("r")
                           ("disk table")
                           "Selects the seventh disk sort column."
                           ("src/app.rs:1221-1229"))
   ("Select disk column 7" ("w")
                           ("disk table")
                           "Selects the eighth disk sort column."
                           ("src/app.rs:1230-1238"))
   ("Sort temperatures by name" ("s")
                                ("temperature table")
                                "Selects temperature-name sorting."
                                ("src/app.rs:1199-1210"))
   ("Sort temperatures by value" ("t")
                                 ("temperature table")
                                 "Selects temperature-value sorting."
                                 ("src/app.rs:1177-1194"))
   ("Select help section" ("1" "2" "3" "4" "5" "6" "7" "8" "9")
                          ("help")
                          "Jumps to a numbered section, when present."
                          ("src/app.rs:949-959"))
   ("Scroll help" ("j" "k")
                  ("help")
                  "Scrolls help one line."
                  ("src/app.rs:961-962; src/app.rs:1907-1918"))
   ("Documented horizontal movement" ("h" "l")
                                     ("normal process table")
                                     "Help claims h/l movement; source and runtime show no binding."
                                     ("src/app.rs:1022-1253"
                                      "runtime: in-app help and frozen process-table key probe")))

(in-package #:ntop.e2e)

(defun %route-manual-procedure (route subcase input context)
  (let ((id (route-id route))
        (kind (route-kind route))
        (action (route-action route)))
    (labels ((text (&rest parts)
               (format nil "~{~a~^ ~}" parts))
             (key-procedure ()
               (format nil "~a: ~a" input
                       (cond
                         ((string= action "Freeze updates")
                          (text "Before: record a changing CPU or network metric."
                                "Action: press f. After one sample interval, verify"
                                "the value stays fixed; press f again and verify it resumes."))
                         ((string= action "Expand widget")
                          (text "Before: select a widget and note its bounds."
                                "Action: press e. After: verify it expands; press Escape"
                                "and verify the normal layout returns."))
                         ((string= action "Reset collection")
                          (text "Before: record Total RX and Total TX in Network."
                                "Action: press Ctrl+R. After: verify collection restarts"
                                "and the totals reflect the reset."))
                         ((and (string= id "key-9")
                           (string= action "Move widget selection right"))
                          (text "Before: use BTM at 60x220 with its default layout; confirm"
                                "CPU and Memory share the first row and Network, Disks, and"
                                "Temperatures are visible below. Identify the active widget"
                                "by its title or border. Action: send"
                                (getf subcase :input)
                                "After: verify the active title or border moves one widget"
                                "to the right. If no focus mark is visible, record that the"
                                "active widget could not be confirmed."))
                         ((and (member id '("key-10" "key-11") :test #'string=)
                               (search "Move widget selection" action))
                          (text "Before: use a layout with at least two widget rows and columns."
                                "Select a widget with a neighbor in the requested direction and"
                                "note its selected border. Action: send"
                                (getf subcase :input) "once."
                                "After: verify the adjacent widget's border is selected."
                                "If focus has no visible marker, record that limitation."))
                         ((search "Move widget selection" action)
                          (text "Before: focus an interior widget in a layout with at least"
                                "two widgets and note its selected border. Action: send"
                                (getf subcase :input)
                                "After: verify the border moves one position in the"
                                "documented direction."))
                         ((member action '("Go to first row" "Go to last row"
                                          "Move to previous row" "Move to next row"
                                          "Page up" "Page down" "Half-page up"
                                          "Half-page down")
                                  :test #'string=)
                          (text "Before: in" context
                                "scroll away from the relevant boundary and note the"
                                "first visible row or heading. Action: send"
                                (getf subcase :input)
                                "After: verify the viewport or row selection moves for"
                                action "as expected."))
                         ((string= action "Toggle search case")
                          (text "Before: open" context "search, enter mixed-case query"
                                "`Sleep`, and note the matching rows or case indicator."
                                "Action: send" (getf subcase :input)
                                "After: verify the case-sensitive match or indicator changes."))
                         ((and (string= id "key-27")
                               (string-equal context "process search")
                               (string= (getf subcase :input) "F3"))
                          (text "Before: start at least two unrelated sleep processes and"
                                "confirm their Name rows are visible. Open Process search,"
                                "enter ^sleep$, and verify literal mode shows no matching rows."
                                "Action: press F3. After: verify the matching sleep rows appear"
                                "in regex mode; press F3 again and verify they disappear, then"
                                "press Escape to close search. If Processes shows No data,"
                                "record that no host rows are available."))
                         ((member action '("Move search cursor to start"
                                          "Move search cursor to end" "Clear search"
                                          "Delete previous word"
                                          "Delete previous character"
                                          "Delete next character")
                                  :test #'string=)
                          (text "Before: open the" context "search field, enter"
                                "`cpu sleep`, and place the caret where" action
                                "changes the query. Action: send"
                                (getf subcase :input)
                                "After: verify the query text or caret position."))
                         ((and (member id '("key-20" "key-21") :test #'string=)
                               (string-equal context "battery tabs"))
                          (text "Before: open Battery and note its active tab label or"
                                "highlight. Action: press"
                                (getf subcase :input)
                                "once. After: verify the selected tab visibly moves."
                                "If only the Battery heading appears with no selector,"
                                "record that no tab state is available to observe."))
                         ((string= action "Open process search")
                          (text "Before: select Processes. Action: press /. After:"
                                "verify the process search prompt and input cursor."))
                         ((string= action "Open help search")
                          (text "Before: open Help and note its first heading."
                                "Action: press ? then /. After: verify the Help"
                                "search prompt is visible."))
                         ((search "Sort process by" action)
                          (text "Before: show two processes with different values for"
                                action "and note their order. Action: press"
                                (getf subcase :input)
                                "After: verify the matching sort header and row order."))
                         ((string= action "Toggle process command display")
                          (text "Before: show process rows and note the Name header."
                                "Action: press P. After: verify Command replaces Name."))
                         ((string= action "Reverse sort order")
                          (text "Before: show two process rows and note their order."
                                "Action: press I. After: verify the same rows appear"
                                "in reverse order."))
                         ((string= action "Toggle process tree")
                          (text "Before: show a parent process with a child and note its"
                                "indentation. Action: press t or F5. After: verify the"
                                "tree indentation or child visibility toggles."))
                         ((string= action "Toggle process tree branch")
                          (text "Before: show an expanded process branch and its children."
                                "Action: send" (getf subcase :input) "on the parent."
                                "After: verify that branch collapses or expands."))
                         ((member action '("Zoom graph in" "Zoom graph out"
                                          "Reset graph zoom")
                                  :test #'string=)
                          (text "Before: show the CPU graph and record its primary-axis"
                                "window. Action: press" (getf subcase :input)
                                "After: verify the axis changes for" action
                                "while the adjacent subplot scale stays unchanged."))
                         ((string= action "Toggle memory values")
                          (text "Before: show process rows with the Mem% header."
                                "Action: press %. After: verify the header toggles"
                                "between Mem% and Mem."))
                         ((string= action "Toggle kernel threads")
                          (text "Before: on Linux, show kernel-thread rows. Action:"
                                "press z. After: verify kernel-thread rows appear or disappear."))
                         ((search "Select disk column" action)
                          (text "Before: show the Disks table and note its selected"
                                "header. Action: press" (getf subcase :input)
                                "After: verify the requested disk column is highlighted."))
                         ((search "Sort temperatures by" action)
                          (text "Before: show two temperature rows with different names"
                                "or values. Action: press" (getf subcase :input)
                                "After: verify rows follow the requested order."))
                         ((string= action "Select help section")
                          (text "Before: open Help at a different numbered section and"
                                "note its heading. Action: press" (getf subcase :input)
                                "After: verify that numbered section is shown first."))
                         ((string= action "Scroll help")
                          (text "Before: open Help away from the"
                                (if (string= (getf subcase :input) "k") "top" "bottom")
                                "boundary and note the first heading. Action: press"
                                (getf subcase :input)
                                "After: verify the viewport moves one line."))
                         ((string= action "Toggle process table tab")
                          (text "Before: show process rows and note the selected tab."
                                "Action: press Tab. After: verify the other tab becomes active."))
                         ((string= action "Delete selected process")
                          (text "Before: confirm the disposable child's PID and"
                                "ntop-owned marker are on the same row. Action: press"
                                "Delete. After: verify the dialog names that PID,"
                                "then cancel without terminating it."))
                         ((string= action "Open process kill dialog")
                          (text "Before: confirm the disposable child's PID and"
                                "ntop-owned marker are on the same row. Action: press"
                                "dd or F9. After: verify the dialog names that PID,"
                                "then cancel without terminating it."))
                         ((string= action "Accept current choice")
                          (cond
                            ((string-equal context "process search")
                             (text "Before: show matching and nonmatching process rows."
                                   "Open Process search, enter a query that selects a visible"
                                   "row, and note the result. Action: press Enter."
                                   "After: verify the prompt closes and the matching row remains."))
                            ((string-equal context "process sort")
                             (text "Before: show at least two rows with different CPU values."
                                   "Open Sort By and highlight CPU. Action: press Enter."
                                   "After: verify the menu closes and rows follow CPU order."))
                            ((string-equal context "kill dialog")
                             (text "Before: confirm the child PID and ntop-owned marker share"
                                   "one row and the dialog names that PID. Do not press Enter."
                                   "Action: press Escape or choose No. After: verify the dialog"
                                   "closes and the same child remains alive."))
                            (t
                             (text "Before: in" context
                                   "note the selected choice. Action: press Enter."
                                   "After: verify that choice is applied."))))
                         ((string= action "Close current modal")
 (cond
   ((string-equal context "kill dialog")
    (text "Before: confirm the child PID and ntop-owned marker are"
          "on the same row and the prompt names that PID."
          "Action: press Escape. After: verify the prompt closes"
          "and the same child remains alive."))
   ((string-equal context "process sort")
    (text "Before: open Sort By and note its highlighted option."
          "Action: press Escape. After: verify the menu closes and"
          "the Processes table returns."))
   ((string-equal context "process search")
    (text "Before: open Process search and enter a visible query."
          "Action: press Escape. After: verify the prompt closes"
          "and the Processes view returns."))
   ((string-equal context "help search")
    (text "Before: open Help search and enter process."
          "Action: press Escape. After: verify the search prompt"
          "closes and the Help page returns."))
   ((string-equal context "help")
    (text "Before: open Help with ? and verify Esc to close is shown."
          "Action: press Escape once. After: verify Help closes and"
          "the dashboard returns. Record the BTM version and result."))
   (t
    (text "Before: open the" context "modal and note its prompt."
          "Action: press Escape. After: verify it closes and"
          "the underlying view returns."))))
                         (t
                          (cond
  ((and (or (string= id "key-20") (string= id "key-21"))
        (string-equal context "process tree"))
   (if (string= id "key-20")
       (text "Before: show an expanded Processes tree and select a parent with a visible child."
             "Action: send" (getf subcase :input)
             "After: verify the child row collapses under that parent.")
     (text "Before: show a collapsed Processes parent with a child and select the parent."
           "Action: send" (getf subcase :input)
           "After: verify the child row appears indented under it.")))
  ((and (or (string= id "key-20") (string= id "key-21"))
        (string-equal context "search cursor"))
   (if (string= id "key-20")
       (text "Before: open Process search, enter sleep with the caret at the end. Action: send"
             (getf subcase :input) "then type X. After: verify the query reads sleeXp."
             "Remove X and close with Escape.")
     (text "Before: enter sleep in Process search, then press Left to place the caret before p."
           "Action: send" (getf subcase :input) "then type X. After: verify sleepX."
           "Remove X and close with Escape.")))
  ((and (or (string= id "key-20") (string= id "key-21"))
        (string-equal context "kill dialog"))
   (text "Before: confirm the disposable PID and ntop-owned marker share one row. Select it,"
         "open its kill dialog, and note the Yes/No highlight. Action: send"
         (getf subcase :input) "After: verify the other choice is highlighted; press Escape"
         "and confirm the same PID remains alive. Without same-row evidence, do not confirm."))
  ((and (or (string= id "key-26") (string= id "key-27"))
        (string-equal context "help search"))
   (text "Before: open Help search, enter process, and record visible matching lines. Action: send"
         (getf subcase :input) "After: verify the prompt and matches remain unchanged, then"
         "press Escape."))
  ((and (or (string= id "key-26") (string= id "key-27"))
        (string-equal context "process search")
        (string= action "Toggle whole-word search"))
   (text "Before: show process rows named sleep and sleepy. Search sleep and note result rows"
         "and Whole(F2). Action: send" (getf subcase :input)
         "After: verify the marker and whole-word result rows; press Escape."))
  ((and (or (string= id "key-26") (string= id "key-27"))
        (string-equal context "process search")
        (string= action "Toggle regex search"))
   (text "Before: show sleep and sleepy process rows. Search ^sleep$ in literal mode and note"
         "result rows and Regex(F3). Action: send" (getf subcase :input)
         "After: verify the marker and exact sleep row; press Escape."))
  (t (text "Before: open" context "and note the state for" action ". Action: send"
            (getf subcase :input) "After: verify" (route-expected route) ".")))))))) (cond
        ((string= id "mouse-74")
         (text "Before: show at least two process rows whose Name and CPU order differ."
               "Open Sort By, choose CPU, and note row order. Action: click Name."
               "After: verify alphabetical order. If Processes shows No data, record"
               "that no rows are available and stop."))
        ((and (member id '("mouse-69" "mouse-70") :test #'string=)
              (string-equal context "graphs"))
         (if (string= id "mouse-69")
             (text "Before: show CPU graph at a 60s primary-axis window."
                   "Action: wheel up over the plot. After: verify primary axis changes"
                   "to 45s and adjacent subplot scale stays unchanged.")
             (text "Before: show CPU graph at a 45s primary-axis window."
                   "Action: wheel down over the plot. After: verify primary axis changes"
                   "to 60s and adjacent subplot scale stays unchanged.")))
        ((and (member id '("mouse-69" "mouse-70") :test #'string=)
              (string-equal context "help"))
         (if (string= id "mouse-69")
             (text "Before: open Help below the top and note the first visible heading."
                   "Action: wheel up over Help. After: verify an earlier heading moves"
                   "into view.")
           (text "Before: open Help at the General heading and note the first visible row."
                 "Action: wheel down over Help. After: verify the viewport moves down.")))
        ((eq kind :key) (key-procedure))
        ((and (eq kind :mouse) (string= id "mouse-73"))
         (text "Before: start a disposable harness-owned child; confirm its numeric PID"
               "and ntop-owned marker appear on the same Processes row. Select that PID"
               "and note the row highlight. Action: click that exact row once. After:"
               "verify the row highlight reflects selection, no kill dialog opens,"
               "confirm the same PID remains alive; do not terminate it."))
        ((and (eq kind :mouse) (string= id "mouse-78"))
         (text "Before: start a disposable harness-owned child and confirm its numeric PID"
               "and ntop-owned marker appear on the same Processes row. Select that PID,"
               "open the kill dialog, and verify it names that PID. Action: click No."
               "After: verify the dialog closes, no dialog remains, and the same PID"
               "stays alive. Do not terminate it."))
        ((and (eq kind :mouse) (string-equal context "CPU legend"))
         (text "Before: show at least two named CPU series. Action: click a different"
               "series. After: verify its selected label or plotted line."))
        ((and (eq kind :mouse) (string-equal context "temperature table"))
         (text "Before: use a host with temperature sensor rows. Action: click the"
               "Sensor header or row. After: verify the sorting or selection change."))
        ((and (eq kind :mouse) (string-equal context "disk table"))
         (text "Before: use a host with mounted disks. Action: click a Disk header or"
               "row. After: verify sorting or selection changes."))
        ((and (eq kind :mouse) (string-equal context "battery widget"))
         (text "Before: use a host with at least two selectable battery tabs."
               "Action: click a different tab. After: verify selection changes."))
        ((and (eq kind :mouse)
              (member context '("tables" "process table") :test #'string-equal))
         (text "Before: show live process rows and note the first visible row."
               "Action: wheel over the process table. After: verify its scroll position."))
        ((string= id "mouse-71")
         (text "Before: show at least two widgets and note the selected border."
               "Action: click an unselected widget. After: verify its border becomes selected."))
        ((string= id "mouse-72")
         (text "Before: start basic mode and note the selected-widget marker."
               "Action: click the next-widget arrow. After: verify the marker moves."))
        ((and (eq kind :flag)
              (string= (ntop.app.domain:parity-flag-snake-case (route-entry route))
                       "disable_advanced_kill"))
         (text "Before: confirm a disposable child PID and ntop-owned marker share one"
               "visible row. Action: press Delete. After: verify the child stays alive"
               "and the dialog offers only Yes/No; do not confirm termination."))
        ((and (eq kind :flag)
              (string= (ntop.app.domain:parity-flag-snake-case (route-entry route))
                       "read_only"))
         (text "Before: confirm a disposable child PID and ntop-owned marker share one"
               "visible row. Action: press Delete. After: verify the child stays alive"
               "and no kill dialog opens."))
        ((eq kind :column)
         (format nil
                 (concatenate
 'string
 "Before: select Processes and start a disposable harness-owned sleep child; confirm "
 "its PID and ntop-owned marker appear on the same row. Action: inspect header ~s "
 "against condition ~a. After: record the header result. If BTM shows No data or "
 "the PID and marker are not on one row, stop; do not terminate a process.")
                 (ntop.app.domain:parity-column-header (route-entry route))
                 (ntop.app.domain:parity-column-condition (route-entry route))))
        ((eq kind :search-field)
         (format nil "Before: open Processes search. Action: enter ~s in field ~a."
                 (getf subcase :input)
                 (ntop.app.domain:parity-search-field-field (route-entry route))))
        ((string= id "cpu-frequency")
         (text "Before: record CPU frequency averages by cluster. Action: run"
               "sudo powermetrics --samplers cpu_power -i 1000 -n 5 beside ntop."
               "After: compare each cluster average within 10%."))
        ((string= id "basic-mode")
         (text "Before: note the normal widget layout. Action: run btm --basic."
               "After: verify RAM, RX/TX totals, Disks, and Temperature appear"
               "and no process table is shown."))
        ((string= id "sleep-child")
         (text "Before: start sleep 120 through the ntop harness and show its process row."
               "Confirm its numeric PID and ntop-owned marker share that same row."
               "Action: select that PID and confirm stop only after rechecking the same-row marker."
               "After: verify only that PID exits and an unrelated process remains alive."
               "If same-row evidence is absent, do not confirm."))
        (t
         (cond
           ((and (string= id "extension-170") (string= context "Memory"))
            (text "Before: select Memory and record its contents. Action: press v twice."
                  "After: verify GPU data replaces the Memory view."))
           ((and (string= id "extension-170") (string= context "Temperatures"))
            (text "Before: select Temperatures and note its table. Action: press v once"
                  "at a time. After: verify a graph after the first press and Battery after"
                  "the third."))
           ((and (string= id "extension-170") (string= context "Disks"))
            (text "Before: select Disks and note its table. Action: press v twice."
                  "After: verify the I/O graph appears."))
           ((string= id "extension-171")
            (text "Before: show the CPU graph at the current time and note its time axis."
                  "Action: press [ once. After: verify history shifts back one zoom-width."))
           ((string= id "extension-172")
            (text "Before: press [ once in the CPU graph and note the earlier time axis."
                  "Action: press ]. After: verify the view advances one zoom-width."))
           ((string= id "extension-173")
            (text "Before: move the CPU graph into the past with [ and note its time axis."
                  "Action: press G. After: verify the graph returns to current time."))
           ((string= id "restore-exception")
            (text "Before: record stty -g for the PTY. Action: run a PTY test whose"
                  "thunk raises a known error inside call-with-terminal-session. After:"
                  "verify the error escapes and stty -g matches the saved value."))
           (t
            (text "Before: establish the documented starting state for" id
                  ". Action: run" input ". After: verify" (route-expected route) ".")))))))
)
(defun %route-btm-self-manual-spec (mode route subcase)
  (when (string= mode "btm-self")
    (let ((id (route-id route))
          (context (getf subcase :context))
          (input (getf subcase :input))
          (variant (getf subcase :variant)))
      (labels ((text (&rest parts)
                 (format nil "~{~a~^ ~}" parts)))
        (cond
          ((and (string= id "key-15")
                (string= context "tables")
                (string= input "j"))
           (list :reason
                 (text "The BTM-self tables/j case hit an E2E close timeout"
                       "after 1.000 seconds. The capture does not establish"
                       "whether BTM completed row movement before the PTY close check.")
                 :procedure
                 (text "Start BTM in its default 60 by 220 layout. In Processes,"
                       "select a row that has a following row and record the"
                       "selected row and nearby labels. Press j once and verify"
                       "that selection advances one row or the viewport moves."
                       "Quit with q; record whether the PTY closes and terminal"
                       "settings are restored.")))
          ((and (string= id "flag-113")
                 (eq variant :kebab)
                 (not (getf subcase :invalid-value)))
           (list :reason
                 (text "The first BTM-self --kelvin spelling hit an E2E close"
                       "timeout after 1.000 seconds. Sibling spellings completed,"
                       "but this run does not establish the result for this exact"
                       "spelling.")
                 :procedure
                 (text "Launch BTM with --kelvin. Inspect the temperature values"
                       "and confirm that their displayed unit is Kelvin. Quit"
                       "with q and record whether the PTY closes and terminal"
                       "settings are restored.")))
          ((and (string= id "search-field-153") (string= input "pid"))
           (list :reason
                 (text "The BTM-self captures had matching condition-A projections,"
                       "but the runner did not observe the expected pid 0 query"
                       "on either side. Query acceptance remains unverified.")
                 :procedure
                 (text "Open Processes search in BTM and enter exactly pid 0."
                       "Verify that the prompt visibly retains pid 0 without a"
                       "syntax error, record the resulting rows, and close the"
                       "prompt with Escape.")))
          ((and (string= id "search-field-154") (string= input "cpu"))
           (list :reason
                 (text "The BTM-self captures had matching condition-A projections,"
                       "but the runner did not observe the expected cpu>=0 query"
                       "on either side. Query acceptance remains unverified.")
                 :procedure
                 (text "Open Processes search in BTM and enter exactly cpu>=0."
                       "Verify that the prompt visibly retains cpu>=0 without a"
                       "syntax error, record the resulting rows, and close the"
                       "prompt with Escape.")))
          ((and (string= id "search-field-155") (string= input "memb"))
           (list :reason
                 (text "The BTM-self memb>=0 case hit an E2E close timeout after"
                       "1.000 seconds. This run does not establish whether BTM"
                       "accepted the query before the PTY close check.")
                 :procedure
                 (text "Open Processes search in BTM and enter exactly memb>=0."
                       "Verify that the prompt visibly retains memb>=0 without a"
                       "syntax error, record the resulting rows, and close the"
                       "prompt with Escape. Record whether BTM exits and restores"
                       "the terminal.")))
          ((and
            (member id (list "key-51" "key-52" "key-53"
                             "mouse-69" "mouse-70")
                    :test (function string=))
            (or (not (member id (list "mouse-69" "mouse-70")
                             :test (function string=)))
                (string-equal (or context "") "graphs")))
           (list :reason
                 (text "BTM-self condition-A matched, but only one launch showed"
                       "the expected graph zoom transition. The run cannot"
                       "establish consistent behavior; verify it manually.")
                 :procedure
                 (cond
                   ((string= id "key-51")
                    (text "At 60s, press +; verify CPU changes to 45s and the"
                          "adjacent scale stays fixed. Repeat in a fresh BTM."))
                   ((string= id "key-52")
                    (text "At 45s, press -; verify CPU returns to 60s and the"
                          "adjacent scale stays fixed. Repeat in a fresh BTM."))
                   ((string= id "key-53")
                    (text "Zoom in with +, then press =; verify CPU resets to"
                          "60s and the adjacent scale stays fixed. Repeat fresh."))
                   ((string= id "mouse-69")
                    (text "At 60s, wheel up over the CPU plot; verify it changes"
                          "to 45s and adjacent scale stays fixed. Repeat fresh."))
                   (t
                    (text "At 60s, wheel up then down over the CPU plot; verify"
                          "45s then 60s while adjacent scale stays fixed. Repeat"
                          "in a fresh BTM process.")))))
)))))

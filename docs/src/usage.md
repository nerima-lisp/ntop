# Usage

## Widgets and views

The default screen includes CPU, memory, network, disk, temperature, and process information. Use the normal-screen navigation keys below to select a widget. Press `v` on the selected widget to cycle views where available: memory switches to GPU, temperature switches to a graph and then battery, and disks switch to an I/O graph.

## Keyboard controls

| Context | Keys | Action |
| --- | --- | --- |
| Normal screen | `q`, `Q`, `Shift+Q` | Quit |
| Any screen | `Ctrl+C` | Quit, including while search is open |
| Normal screen | `?` | Open help |
| Normal screen | `f` | Freeze or resume collection |
| Normal screen | `Ctrl+R` | Reset collected data |
| Normal screen | `e` | Expand or restore the selected widget |
| Normal screen | `v` | Cycle the selected widget's view where available |
| Normal screen | `Ctrl+Left`, `Shift+Left`, `H`, `A` | Select the widget to the left |
| Normal screen | `Ctrl+Right`, `Shift+Right`, `L`, `D` | Select the widget to the right |
| Normal screen | `Ctrl+Up`, `Shift+Up`, `K`, `W` | Select the widget above |
| Normal screen | `Ctrl+Down`, `Shift+Down`, `J`, `S` | Select the widget below |
| Tables and help | `Up`, `k`; `Down`, `j` | Move one row |
| Tables and help | `Home`, `gg`; `End`, `G` | Move to the first or last row |
| Tables, help, and kill dialog | `PageUp`, `PageDown` | Move one page |
| Tables and help, no search field open | `Ctrl+U`, `Ctrl+D` | Move half a page up or down |
| Process search field | `Left`, `Alt+h` | Move the query cursor one character left |
| Process search field | `Right`, `Alt+l` | Move the query cursor one character right |
| Process table or sort list | `/`, `Ctrl+F` | Open process search |
| Help | `/`, `Ctrl+F` | Search help |
| Process search | `F1`, `Alt+c`, `Alt+C` | Toggle case sensitivity |
| Process search | `F2`, `Alt+w`, `Alt+W` | Toggle full-field matching |
| Process search | `F3`, `Alt+r`, `Alt+R` | Toggle regular-expression matching |
| Process search | `Ctrl+A`, `Ctrl+E` | Move the query cursor to the start or end |
| Process search | `Ctrl+U` | Clear the query |
| Process search | `Ctrl+W` | Delete the previous word |
| Process search | `Backspace`, `Ctrl+H` | Delete the previous character |
| Process search | `Delete` | Delete the next character |
| Help search | `Ctrl+U` | Clear the query |
| Help search | `Ctrl+W` | Delete the previous word |
| Help search | `Backspace`, `Ctrl+H` | Delete the previous character |
| Search, help search, sort, or kill dialog | `Enter` | Submit a search, apply sorting, or confirm the selected kill action |
| Dialog or expanded widget | `Escape` | Close the dialog or restore the expanded widget |
| Process table | `Tab` | Toggle the process table tab |
| Process table | `c`, `m`, `p`, `n`, `C`, `M` | Sort by CPU, memory, PID when ungrouped or process count when grouped, name, GPU use, or GPU memory (GPU keys when GPU support is available) |
| Process table | `P`, `%`, `z` | Toggle command display, memory values or percentages, or Linux kernel-thread visibility |
| Process table | `s`, `F6`; `I`; `t`, `F5` | Open sort choices, reverse sort order, or toggle process-tree mode |
| Process table | `Delete`, `dd`, `F9` | Open the process kill dialog; `dd` is a key sequence |
| Process tree | `Space`, `+`, `-` | Toggle the selected branch |
| Disk table | `d`, `m`, `u`, `n`, `t`, `p`, `r`, `w` | Sort by Disk, Mount, Used, Free, Total, Used%, read rate, or write rate, respectively |
| Temperature table | `s`, `t` | Sort by name or value |
| Graphs | `+`, `-`, `=` | Zoom in, zoom out, or reset zoom |
| Help | `1` through `9` | Jump to a numbered section when present |

## Mouse controls

In the process table, click a row to select it. In the process sort list, click a choice to apply it. In the kill dialog, click a choice to select it, then press `Enter` to confirm.

On graphs, scroll up or down to zoom in or out. In the process table and help, the wheel scrolls; in the kill dialog, it moves through the choices. Right and middle clicks, pointer movement, dragging, and release events have no action. `--disable-click` disables mouse capture; `--disable-keys` disables keyboard controls.

## Process search

Open process search with `/` or `Ctrl+F` while the process table or its sort list is active. A plain term searches the process name, or the command when command display is enabled. Quote a phrase to search text containing spaces.

Search terms can be combined with implicit AND, `AND` or `&&`, and `OR` or `||`. `AND` binds more tightly than `OR`. Use `!` to negate a term and parentheses to group expressions. For example:

```text
worker
"node worker"
cpu% > 25 memb >= 512 MiB
cpu% > 25 AND memb >= 512 MiB
state=running OR user=alice
time < 2m
```

Text fields accept `=` or `!=` followed by a value. Text matches are substrings by default; `!=` negates the match. Numeric fields require a comparison operator: `=`, `!=`, `<`, `>`, `<=`, or `>=`. Byte fields accept decimal or binary units such as `MB` or `MiB`. The `time` field accepts durations such as `2m` or `90s`.

| Search field | Names |
| --- | --- |
| Process ID | `pid` |
| CPU percentage | `cpu`, `cpu%` |
| Memory bytes | `memb` |
| Memory percentage | `mem`, `mem%` |
| Read and write rates | `read`, `r/s`, `rps`; `write`, `w/s`, `wps` |
| Total read and write | `tread`, `t.read`; `twrite`, `t.write` |
| Text fields | `state`, `user` |
| Time, nice value, and priority | `time`, `nice`, `priority` |
| GPU use and memory | `gpu%`, `gmem`, `gmem%` when GPU support is available |

Search is case-insensitive by default. `F1` toggles case sensitivity, `F2` toggles full-field matching, and `F3` toggles regular-expression matching. The corresponding startup flags are `--case-sensitive`, `--whole-word`, and `--regex`. `--whole-word` requires an expression to match the entire field value; it does not match a word boundary inside a longer value.

## Basic mode

Run `ntop --basic` or `ntop -b` to use basic mode. It shows CPU, memory, network, and a process table. In basic mode, `--expanded` has no effect.

## Graph history

On a graph, press `[` to move back one visible time window and `]` to move forward one window. These keys are ntop additions. Press `G` on a graph to return to current data; in tables and help, `G` moves to the last row.

## Command-line options

Long options accept both hyphenated and underscore spellings. Options marked with a value placeholder require that value; the other options take no value. The table after the option list gives accepted forms and choices. `ntop --help` prints the options recognized by the executable.

| Area | Options |
| --- | --- |
| Display and time | `--autohide-time`, `--basic` (`-b`), `--default-time-value <duration>` (`-t`), `--default-widget-count <integer>`, `--default-widget-type <type>`, `--disable-click`, `--disable-keys`, `--dot-marker` (`-m`), `--expanded` (`-e`), `--hide-time`, `--rate <duration>` (`-r`), `--retention <duration>`, `--show-table-scroll-position`, `--time-delta <duration>` (`-d`) |
| Process table and search | `--case-sensitive` (`-S`), `--current-usage` (`-u`), `--disable-advanced-kill`, `--read-only`, `--hide-k-threads`, `--get-threads`, `--process-default-sort <column>`, `--group-processes` (`-g`), `--process-memory-as-value`, `--process-command`, `--regex` (`-R`), `--tree` (`-T`), `--tree-collapse`, `--unnormalized-cpu` (`-n`), `--whole-word` (`-W`) |
| CPU, memory, and temperature | `--celsius` (`-c`), `--fahrenheit` (`-f`), `--kelvin` (`-k`), `--cpu-left-legend` (`-l`), `--default-cpu-entry <entry>`, `--hide-avg-cpu` (`-a`), `--memory-legend <position>`, `--enable-cache-memory`, `--free-arc`, `--free_arc` (accepted; no display effect; see [Differences from btm](differences.md)) |
| Network, GPU, battery, and theme | `--short-gpu-names`, `--network-legend <position>`, `--network-use-bytes`, `--network-use-binary-prefix`, `--network-use-log`, `--use-old-network-legend`, `--show-packets`, `--network-start-zeroed`, `--battery`, `--disable-gpu`, `--theme <name>` |
| CLI actions | `--help` (`-h`), `--version` (`-V`) |
| Rejected compatibility option | `--config-location <path>`, `--config_location <path>`, `--config <path>` (`-C`) (always exits with status 2) |

Duration values accept an integer with or without a unit. A value without a unit is milliseconds. Units include `ns`, `us`, `ms`, `s`, `m`, `h`, `d`, `w`, and `y`; full singular and plural names and common abbreviations are also accepted. Examples are `700ms`, `15s`, and `1m`. Value options use these forms and choices:

| Option | Value |
| --- | --- |
| `--default-time-value`, `--rate`, `--retention`, `--time-delta` | Duration |
| `--default-widget-count` | Integer; requires `--default-widget-type`; ntop ignores the value |
| `--default-widget-type` | `cpu`, `mem` or `memory`, `net` or `network`, `proc`, `process`, or `processes`, `temp` or `temperature`, `disk`, `batt` or `battery` |
| `--process-default-sort` | Process-table column alias, such as `cpu%`, `mem`, `pid`, or `name` |
| `--default-cpu-entry` | `all`, `avg`, or `average` |
| `--memory-legend`, `--network-legend` | `none`, `top-left`, `top`, `top-right`, `left`, `right`, `bottom-left`, `bottom`, or `bottom-right` |
| `--theme` | `default`, `default-light`, `gruvbox`, `gruvbox-light`, `nord`, or `nord-light` |

The following options set behavior that is not clear from their names:

| Options | Effect |
| --- | --- |
| `--rate`, `--retention`, `--default-time-value`, `--time-delta` | Set the sampling interval, retained history, initial graph window, and graph zoom step, respectively. |
| `--autohide-time`, `--hide-time`, `--dot-marker` | Automatically hide network-graph time labels when its height is under six rows; always hide those labels; or mark chart samples with dots. |
| `--default-cpu-entry`, `--cpu-left-legend`, `--hide-avg-cpu` | Start the CPU widget with the all or average entry selected; place the CPU graph legend on the left; or omit the average CPU entry. |
| `--enable-cache-memory`, `--short-gpu-names` | Collect cache-memory figures where supported; shorten GPU series names. |
| `--group-processes`, `--process-command`, `--process-memory-as-value`, `--tree`, `--tree-collapse`, `--unnormalized-cpu` | Group rows by process name, or by command when `--process-command` is set; show commands instead of names; show memory as bytes instead of percentages; enable process-tree mode; start branches collapsed when tree mode is enabled; or disable process CPU normalization by CPU capacity. |
| `--read-only`, `--disable-advanced-kill` | Prevent process termination; use a Yes/No confirmation instead of signal choices on Unix-like systems. |
| `--network-use-bytes`, `--network-use-binary-prefix`, `--network-use-log`, `--network-start-zeroed`, `--show-packets`, `--use-old-network-legend` | Use bytes instead of bits; use binary instead of decimal prefixes; use a logarithmic network-chart scale; subtract the first sample's counters from totals so they start at zero; include packet rates and average packet sizes where the layout has room; or use the legacy network chart legend. |
| `--memory-legend`, `--network-legend` | Set the legend position for the memory or network chart. |

`--current-usage` calculates process CPU percentages as a share of currently active system CPU usage. `--get-threads` includes process threads in collection.

`--celsius`, `--fahrenheit`, and `--kelvin` are mutually exclusive. `--disable-advanced-kill` applies to Unix-like systems. `--hide-k-threads` is Linux-only. `--battery` enables collection for the battery view, which is available from the temperature widget's view cycle. Battery collection is off by default. If no battery sample is available, the view displays `No data found for this battery`. `--disable-gpu` is available in GPU-enabled builds. `--use-old-network-legend` is deprecated but accepted.

`--default-widget-type` sets the initially selected widget; it does not add a widget to the layout. The accepted `battery` value does not switch to the battery view; press `v` on the temperature widget to open it.

ntop does not load configuration files. See [Differences from btm](differences.md#unsupported-behavior) for the rejected compatibility options.

### Defaults

| Setting | Default |
| --- | --- |
| Sampling interval | 700 ms |
| Retained history | 600 seconds |
| Initial graph window | 60 seconds |
| Graph zoom step | 15 seconds |
| Initial widget | Process |
| Process table | Grouped, sorted by CPU in descending order |
| Process search | Case-insensitive, literal matching, full-field matching off |
| Temperature unit | Celsius |
| Network data unit | Bits |
| Mouse and keyboard input | Enabled |

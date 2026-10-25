# WP15b Nonfunctional and Difference Acceptance Measurements

WP15b records local measurements for the WP15 nonfunctional requirements and difference acceptance tests. Measurements were collected on 2026-10-25 with macOS 26.6.2, arm64, and an Apple M4 Max. The native `ntop` executable was used for the macOS runs. PTY tests used 160 columns by 48 rows and a 700 ms refresh rate where applicable. The comparator was btm 0.14.9. Raw evidence remains in local temporary storage and is not included in the repository.

`PASS` means the measured acceptance condition was met. `FAIL` means a complete measurement exceeded its limit or disagreed with its expected value. `INCOMPLETE` means the required observation could not be established. Load values are one-minute averages unless shown as a three-value tuple in 1-, 5-, and 15-minute order. The N4/N5 load gate uses the one-minute values captured immediately before and after each run. For N4/N5, a sample reaches the quiet gate only after at least 250 ms with no further PTY bytes following nonempty output; this gate is separate from confirming a visible change or complete redraw.

## Nonfunctional requirements

| ID | Requirement | Result | Conditions and measured values |
|---|---|---|---|
| N1 | CPU use versus btm | **FAIL** | Five alternating btm/ntop pairs, 60 seconds per run. Each run produced 86 process-group CPU samples. The per-run median CPU values were 17.36–53.72% for btm (median 25.69%) and 19.78–61.74% for ntop (median 38.46%). The difference was +12.76 percentage points (+49.68% relative), exceeding both allowances (+0.5 points or +10%). One-minute load immediately before/after the full run was 72.44/53.79; per-run boundary values ranged from 53.79 to 76.38. |
| N2 | RSS ceiling and growth | **FAIL** | 61 RSS samples at 30-second intervals over 1,800.229 seconds, with display retention set to 10 minutes. RSS was 116.34/215.84/303.88 MiB (min/median/max). At two minutes it was 199.95 MiB; at 30 minutes it was 253.81 MiB, a 26.94% increase. This exceeds both the 200 MiB ceiling and the 5% growth limit. The measurement establishes the failure; its source-level cause was not determined. |
| N3 | `--version` startup time | **INCOMPLETE** | The original compressed executable took 0.4040/0.4361/0.7449 seconds (min/median/max). In a separate 10-run high-load comparison, compressed and uncompressed executables took 80.59/91.34/126.64 ms and 39.76/42.41/74.09 ms, respectively. The uncompressed Mach-O was 100,084,432 bytes larger. A final 10-run uncompressed sample took 50.35/103.59/197.81 ms; 5/10 runs met 100 ms. Load averages before/after that sample were 62.28/75.26/72.29 and 61.53/74.89/72.17. The measurement protocol qualifies a verdict only when one-minute load is below 40; a five-minute monitor found no such window, so the high-load results remain references. A separate instrumented uncompressed build measured option construction at 0.832/1.588/49.855 ms, argument parsing at 0.013/0.018/0.119 ms, version output at 0.011/0.015/0.077 ms, and total Lisp `main` time at 0.878/1.724/49.919 ms (min/median/max). Whole-process time was 65.978/105.168/257.796 ms. Median per-run time outside Lisp `main` was 85.815 ms. This residual combines pre-`main` startup and core loading with process exit and scheduler delays; the measurement cannot isolate core loading. Instrumented-run load averages were 115.78/93.68/81.09 before and 114.83/93.85/81.22 after. |
| N4 | Key-to-visible-response time | **INCOMPLETE** | The earlier harness alternated `?` and Escape; `?` opens help, and Escape does not close it, so the sequence did not test a reversible visible key response. The revised 20-action PTY sample sends `e`, observes raw output for at least 3.5 seconds, and infers the current layout from the visible `Network`/`Processes` headers before each action. It verified 12/20 layout toggles; verified response latencies were 2.41/42.24/3569.18 ms (min/median/max), against a 50 ms median limit. Thirteen output-bearing samples reached the quiet gate. One-minute load before/after was 102.79/143.30, so this remains a high-load reference and does not establish acceptance. |
| N5 | Resize redraw time | **INCOMPLETE** | Twenty PTY resize actions were observed for at least 3.5 seconds each, alternating 49×161 and 48×160. A completion required raw ANSI to clear and home the terminal, write every cell at the requested dimensions, and finish at the bottom-right `┘`. Fourteen actions produced a verified requested-size frame; the other six had no output (3) or a full frame at the wrong dimensions (3). Verified completion latencies were 44.92/427.97/1378.37 ms (min/median/max), against a 100 ms median limit; 17 output-bearing samples reached the quiet gate. One-minute load before/after was 146.22/174.49, so this is a high-load reference and does not establish acceptance. |
| N6 | Terminal-state restoration | **INCOMPLETE** | Normal exit and SIGINT, SIGTERM, and SIGHUP each passed for ntop (exit codes 0, 130, 143, and 129); `stty -a` matched before and after. A source-runtime unhandled Lisp exception also restored the recorded terminal state and exited nonzero; this case ran through the source SBCL runtime, not the built executable. The built executable's exception case was not generated because the harness has no exception-injection seam. For the four ntop exit cases, the mode-query reply was synthesized from observed DEC private-mode events; it was not an independent terminal query response. |

For N1, all five btm runs reported `EPERM` when cleanup attempted to signal the owned process group. Closing the owned PTY then allowed the child to exit; each PID and process group was confirmed absent and reaped. This cleanup warning did not invalidate the recorded CPU samples.

The btm terminal-restoration reference cases did not reach a confirmed raw-mode state, so no restoration verdict is available for those cases. The capture shows a cursor-position request while the PTY remained canonical with echo enabled; canonical buffering is a plausible explanation, but receipt of the harness reply was not confirmed. SIGINT, SIGTERM, and SIGHUP were not exercised in those reference cases. No SIGKILL acceptance case was run.

## [Difference acceptance tests](differences.md)

| ID | Difference | Result | Conditions and measured values |
|---|---|---|---|
| D1 | macOS process write rate | **INCOMPLETE** | Two child-writer attempts targeted 1,000,000 B/s and achieved 999,960.05 and 999,998.69 B/s (min/median/max 999,960.05/999,979.37/999,998.69 B/s). A valid display sample required one process row uniquely containing the synthetic writer token and a parseable W/s value. The display yielded 0 valid samples: 0 of 18 observations in the first attempt and 0 of 6 after enabling the command column; the test requires 20. Verified: the token was absent from both captures. Inferred: the row may have been omitted or displayed with a different command value; the raw capture cannot distinguish these cases. |
| D2 | Linux fractional I/O rate | **PASS** | One Linux fixture test used a 0.7-second interval and a write-byte delta of 7,168 bytes, producing the expected nonzero rate of 10,240 B/s. |
| D4 | User and Time for other-user processes | **PASS** | With a 160-by-48 PTY and 700 ms refresh, 4 visible other-user rows matched `ps -o pid=,ruser=,etime= -A`: User 4/4 exactly and Time 4/4 within 3 seconds. The comparator found 268 other-user processes; 13 process rows were visible. The screen was captured after 15.62 seconds. The 3-second allowance covers the UI refresh and comparator scan delay. PTY cleanup required SIGTERM and SIGKILL; the `ntop` child was reaped. |
| D5 | Unavailable process values | **PASS** | A privileged process displayed `N/A` for Mem%. |
| D6 | APFS volume usage | **PASS** | The system volume displayed 13 GB used. `df -k` reported 12,340,992 KiB used both before and after, consistent with the rounded display. |
| D7 | Named temperature sensors | **PASS** | All 45 named sensor/service records had a corresponding temperature row (45/45); duplicate names used numbered suffixes. |
| D8 | First CPU average | **PASS** | Sixteen workers saturated all 16 logical cores for 2.0015 seconds; per-core utilization was 100.0/100.0/100.0% (min/median/max). ntop started 25.8 ms after the load window ended; the first displayed CPU average was 97% at 3.1445 seconds after launch. The measurement command returned 1 because its cleanup check initially did not confirm the app PID. A later exact PID check found it absent; all workers were confirmed stopped before app launch. |
| D9 | Fixed sampling period | **PASS** | The application-sampler test used a 700 ms fixed period and a simulated 300 ms collection delay. Three live samples remained exactly 700 ms apart on the virtual clock (0 ms period error); the warm-up sample was discarded. |
| D11 | `--free_arc` behavior | **PASS** | `--free_arc` was accepted. With and without the option, the Processes panel in a 160-by-48 terminal had the same structure: title, column headings, and 20 visible rows. The measurement command returned 1 because cleanup was not initially confirmed; later exact PID checks found both app processes absent. |

## WP13 CLI invalid-input parity

The focused comparison used the built `ntop` executable and btm 0.14.9 on macOS. The 90 unique cases include 74 generated invalid inputs and 15 explicit checks, including seven accepted TUI invocations; some arguments appear in multiple groups.

All 90 exit statuses were observed and matched, with no unexpected results. For the 83 captured cases, stdout/stderr presence also matched. For the seven accepted TUI cases, both programs rendered visible screen content through the existing PTY helper; the runner waited at least 3.5 seconds after launch, confirmed a nonempty terminal screen, then sent `q` and observed exit status 0. The PTY combines terminal output, so stdout/stderr placement is not applicable to TUI cases. Captured stream checks compare only whether each stream is nonempty; diagnostic text is not compared.

Reproduce it with `python3 t/e2e/wp13_cli_parity.py --ntop ./ntop --btm "$(command -v btm)" --output /tmp/wp13-cli` after building `./ntop` and selecting btm 0.14.9.

### Case-by-case results

| Case | Arguments | Expected | ntop | btm | Draw | Channels | Result |
|---|---|---:|---:|---:|---|---|---|
| 001 | `["--default_widget_count", "2"]` | 2 | 2 | 2 | n/a | match | pass |
| 002 | `["--default_widget_type", "cpu"]` | 0 | 0 | 0 | both | n/a | pass |
| 003 | `["--rate", "251ms"]` | 0 | 0 | 0 | both | n/a | pass |
| 004 | `["--hide_k_threads"]` | 2 | 2 | 2 | n/a | match | pass |
| 005 | `["--free_arc"]` | 0 | 0 | 0 | both | n/a | pass |
| 006 | `["--battery"]` | 0 | 0 | 0 | both | n/a | pass |
| 007 | `["-h"]` | 0 | 0 | 0 | n/a | match | pass |
| 008 | `["-V"]` | 0 | 0 | 0 | n/a | match | pass |
| 009 | `["--autohide_time=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 010 | `["--basic=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 011 | `["--config_location"]` | 2 | 2 | 2 | n/a | match | pass |
| 012 | `["--config-location"]` | 2 | 2 | 2 | n/a | match | pass |
| 013 | `["--config"]` | 2 | 2 | 2 | n/a | match | pass |
| 014 | `["-C"]` | 2 | 2 | 2 | n/a | match | pass |
| 015 | `["--default_time_value", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 016 | `["--default_time_value", "29999ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 017 | `["--default-time-value", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 018 | `["--default-time-value", "29999ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 019 | `["-t", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 020 | `["-t", "29999ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 021 | `["--default_widget_count", "not-an-integer"]` | 2 | 2 | 2 | n/a | match | pass |
| 022 | `["--default-widget-count", "not-an-integer"]` | 2 | 2 | 2 | n/a | match | pass |
| 023 | `["--default_widget_type", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 024 | `["--default-widget-type", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 025 | `["--disable_click=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 026 | `["--disable_keys=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 027 | `["--dot_marker=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 028 | `["--expanded=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 029 | `["--hide_time=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 030 | `["--rate", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 031 | `["--rate", "249ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 032 | `["-r", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 033 | `["-r", "249ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 034 | `["--retention", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 035 | `["--show_table_scroll_position=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 036 | `["--time_delta", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 037 | `["--time_delta", "999ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 038 | `["--time-delta", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 039 | `["--time-delta", "999ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 040 | `["-d", "abc"]` | 1 | 1 | 1 | n/a | match | pass |
| 041 | `["-d", "999ms"]` | 1 | 1 | 1 | n/a | match | pass |
| 042 | `["--case_sensitive=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 043 | `["--current_usage=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 044 | `["--disable_advanced_kill=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 045 | `["--read_only=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 046 | `["--hide_k_threads=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 047 | `["--get_threads=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 048 | `["--process_default_sort", "not-a-column"]` | 1 | 1 | 1 | n/a | match | pass |
| 049 | `["--process-default-sort", "not-a-column"]` | 1 | 1 | 1 | n/a | match | pass |
| 050 | `["--group_processes=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 051 | `["--process_memory_as_value=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 052 | `["--process_command=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 053 | `["--regex=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 054 | `["--tree=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 055 | `["--tree_collapse=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 056 | `["--unnormalized_cpu=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 057 | `["--whole_word=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 058 | `["--celsius=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 059 | `["--fahrenheit=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 060 | `["--kelvin=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 061 | `["--cpu_left_legend=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 062 | `["--default_cpu_entry", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 063 | `["--default-cpu-entry", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 064 | `["--hide_avg_cpu=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 065 | `["--memory_legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 066 | `["--memory-legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 067 | `["--enable_cache_memory=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 068 | `["--free_arc=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 069 | `["--short_gpu_names=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 070 | `["--network_legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 071 | `["--network-legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 072 | `["--network_use_bytes=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 073 | `["--network_use_binary_prefix=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 074 | `["--network_use_log=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 075 | `["--use_old_network_legend=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 076 | `["--show_packets=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 077 | `["--network_start_zeroed=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 078 | `["--battery=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 079 | `["--disable_gpu=false"]` | 2 | 2 | 2 | n/a | match | pass |
| 080 | `["--theme", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | pass |
| 081 | `["--help=invalid"]` | 2 | 2 | 2 | n/a | match | pass |
| 082 | `["--version=invalid"]` | 2 | 2 | 2 | n/a | match | pass |
| 083 | `["--bogus"]` | 2 | 2 | 2 | n/a | match | pass |
| 084 | `["--theme", "bad"]` | 2 | 2 | 2 | n/a | match | pass |
| 085 | `["--default_widget_type", "foo"]` | 2 | 2 | 2 | n/a | match | pass |
| 086 | `["--rate", "100"]` | 1 | 1 | 1 | n/a | match | pass |
| 087 | `["--rate", "250"]` | 0 | 0 | 0 | both | n/a | pass |
| 088 | `["--rate", "250ms"]` | 0 | 0 | 0 | both | n/a | pass |
| 089 | `["--retention", "59999ms"]` | 0 | 0 | 0 | both | n/a | pass |
| 090 | `["--celsius", "--kelvin"]` | 2 | 2 | 2 | n/a | match | pass |

The valid path argument `--config_location /tmp/config` is outside this invalid-input comparison. btm proceeds to terminal initialization, and its non-TTY failure emits terminal-control bytes on stdout. ntop rejects config-location values before launching the TUI; that behavior is not covered here.

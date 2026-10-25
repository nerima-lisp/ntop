# WP15b Nonfunctional and Difference Acceptance Measurements

WP15b is the local run label for these WP15 nonfunctional and difference acceptance measurements. Measurements were collected on 2026-10-25 with macOS 26.6.2, arm64, and an Apple M4 Max. The native `ntop` executable was used for the macOS runs. PTY tests used 160 columns by 48 rows and a 700 ms refresh rate where applicable. The comparator was btm 0.14.9. Raw captures and JSON remain in local temporary storage and are not committed; their paths below are machine-local and may not exist elsewhere.

The newly measured N3–N6 and D1/D11 rows used the executable from app commit `a837b77f`. Their harness revisions were `282ae333` for N3–N5, `f870c505` for N6, `f74687e2` for D1, and `86945414` for D11. These are local jj revisions, viewable with `jj show <revision>` in this checkout. The earlier rows retain their previous records.

`PASS` means the measured acceptance condition was met. `FAIL` means a complete measurement exceeded its limit or disagreed with its expected value. `INCOMPLETE` means the required observation could not be established. Load values are one-minute averages unless shown as a three-value tuple in 1-, 5-, and 15-minute order. The one-minute load gate (`<40`) applies only to the time-based observations N1, N3, N4, and N5. N1's high-load CPU result passes the paired numerical comparison but does not establish low-load behavior. N2 records load for context but includes every RSS sample in acceptance because unrelated CPU load has little effect on resident memory; filtering samples would break the 30-minute ceiling observation or remove the exact two- and 30-minute growth endpoints. In `paired_high_load` results, paired btm and ntop runs provide context, while ntop is evaluated against its absolute acceptance limit. An N4/N5 sample passes the load gate only when both one-minute readings immediately before and after its run are below 40. If no sample passes, the specified fallback alternates btm and ntop actions; btm results are context only. N4 has no verified btm action equivalent for switching between Network and Processes layouts; N5 uses the same resize action for both programs. For each N4/N5 action, the sample is the raw PTY output collected after the key or resize event. It reaches the quiet gate after at least 250 ms with no further PTY bytes following nonempty output; this gate is separate from confirming a visible change or complete redraw.

## Nonfunctional requirements

| ID | Requirement | Result | Conditions and measured values |
|---|---|---|---|
| N1 | CPU use versus btm | **PASS** | Five alternating btm/ntop pairs, 60 seconds per run, with 86 process-group CPU samples per run. Per-run median CPU values were 7.62–49.55% for btm (median 14.32%) and 10.16–41.81% for ntop (median 15.75%). The difference was +1.42 percentage points (+9.94% relative). The rule passes if either the relative increase is at most 10% or the difference is at most 0.5 percentage points; this result passes by the relative allowance. One-minute load before and after each run ranged from 74.95 to 153.34, so this high-load result does not establish low-load behavior. |
| N2 | RSS ceiling and growth | **PASS** | 61 RSS samples at 30-second intervals over 1,800 seconds, with display retention set to 10 minutes. One-minute load min/median/max was 79.02/108.86/143.83. RSS min/median/max was 73.88/142.11/176.36 MiB, with the maximum below the 200 MiB ceiling. At two minutes, RSS was 172.81 MiB at load 117.06; at 30 minutes, it was 73.88 MiB at load 108.67. Relative RSS change was -57.25%, calculated from the two-minute value as `((73.88 - 172.81) / 172.81) * 100`, below U9's maximum permitted increase of 5%. Ring expansion followed by garbage collection could explain the decline, but this run recorded no measurements that prove that cause. |
| N3 | `--version` startup time | **PASS** | Under the paired-high-load fallback, all three ntop launches met the 100 ms limit at 50.611, 57.211, and 57.686 ms. No ntop run was load-eligible (0/3); paired btm context samples were 30.963, 12.973, and 14.120 ms. One-minute load was about 77.68. Evidence: `/tmp/ntop-wp15b-n3-182544138ba3442faca8d177c0f411f2/n3.json`. |
| N4 | Key-to-visible-response time | **PASS** | Twenty `e` actions each switched between Network and Processes layouts and passed the visible-toggle, latency, quiet, and cleanup checks. Ntop latency was 2.736/8.135/19.474 ms (min/median/max), within the 50 ms median limit. One-minute load at run boundaries was 88.36/132.81, and 0/20 samples were below 40. Under the specified paired fallback, the ntop absolute criterion passed; btm supplied context for 20/20 actions without a verified equivalent layout-switch action. Evidence: `/tmp/ntop-wp15b-n4-db00938151cc44088818261a63304229/n4.json`. |
| N5 | Resize redraw time | **PASS** | Twenty resize actions passed the redraw, latency, quiet, and cleanup checks. Ntop latency was 5.828/23.503/158.582 ms (min/median/max), within the 100 ms median limit. One-minute load at run boundaries was 141.82/119.97, and 0/20 samples were below 40. Under the specified paired fallback, the ntop absolute criterion passed; btm supplied context for 20/20 identical resize actions. Evidence: `/tmp/ntop-wp15b-n5-23f8c97f9e084b50bc7483154630de54/n5.json`. |
| N6 | Terminal-state restoration | **PASS** | All five built-ntop cases passed: normal exit (0), SIGINT (130), SIGTERM (143), SIGHUP (129), and an injected Lisp exception (1). `stty -a` and independent termios snapshots matched before and after every case. The harness observed a DEC private-mode reset and modeled the cursor-status query reply from ntop's captured terminal sequences; this was not an independent terminal response. All recorded target and wrapper PIDs were absent after the run. Load average (1/5/15 minutes) was 83.993/99.893/105.150 before the run and ranged from 83.625–109.852/98.931–102.992/103.548–105.362 during it. Evidence: `/tmp/ntop-wp15b-n6-4s4fcec6/manifest.json`. |

For N1, all five btm runs reported `EPERM` when cleanup attempted to signal the owned process group. Closing the owned PTY then allowed the child to exit; each PID and process group was confirmed absent and reaped. This cleanup warning did not invalidate the recorded CPU samples.

The N6 btm normal, SIGINT, SIGTERM, and SIGHUP references timed out before reaching raw mode. The source-runtime exception reference timed out waiting for raw-mode readiness. These reference cases are outside the acceptance count. SIGKILL was also outside acceptance and received a diagnostic PASS; it left raw mode active, as expected when the process cannot run cleanup.

## [Difference acceptance tests](differences.md)

| ID | Difference | Result | Conditions and measured values |
|---|---|---|---|
| D1 | macOS process write rate | **FAIL** | The complete 20-sample run displayed `0` B/s 12 times and `2,000,000` B/s 8 times; 0/20 samples were within 900,000–1,100,000 B/s. The writer sustained 957,297.70 B/s overall, with 846,711.37/992,472.04/996,984.66 B/s (min/median/max) across screen windows; the 24.2 MB output matched the file length. One-minute load ranged from 114.41 to 123.76. The run exceeded the acceptance limit; the cause of the displayed values was not established. Evidence: `/tmp/ntop-wp15b-d1-final-20261025123931-69101/d1.json`. |
| D2 | Linux fractional I/O rate | **PASS** | One Linux fixture test used a 0.7-second interval and a write-byte delta of 7,168 bytes, producing the expected nonzero rate of 10,240 B/s. |
| D4 | User and Time for other-user processes | **PASS** | With a 160-by-48 PTY and 700 ms refresh, 4 visible other-user rows matched `ps -o pid=,ruser=,etime= -A`: User 4/4 exactly and Time 4/4 within 3 seconds. The comparator found 268 other-user processes; 13 process rows were visible. The screen was captured after 15.62 seconds. The 3-second allowance covers the UI refresh and comparator scan delay. PTY cleanup required SIGTERM and SIGKILL; the `ntop` child was reaped. |
| D5 | Unavailable process values | **PASS** | A privileged process displayed `N/A` for Mem%. |
| D6 | APFS volume usage | **PASS** | The system volume displayed 13 GB used. `df -k` reported 12,340,992 KiB used both before and after, consistent with the rounded display. |
| D7 | Named temperature sensors | **PASS** | All 45 named sensor/service records had a corresponding temperature row (45/45); duplicate names used numbered suffixes. |
| D8 | First CPU average | **PASS** | Sixteen workers saturated all 16 logical cores for 2.0015 seconds; per-core utilization was 100.0/100.0/100.0% (min/median/max). ntop started 25.8 ms after the load window ended; the first displayed CPU average was 97% at 3.1445 seconds after launch. The acceptance condition is the first displayed average under the stated load window. The command returned 1 because its cleanup check initially did not confirm the app PID; a later exact PID check found it absent, and all workers were confirmed stopped before app launch. |
| D9 | Fixed sampling period | **PASS** | The application-sampler test used a 700 ms fixed period and a simulated 300 ms collection delay. Three live samples remained exactly 700 ms apart on the virtual clock (0 ms period error); the warm-up sample was discarded. |
| D11 | `--free_arc` behavior | **INCOMPLETE** | With and without `--free_arc`, each run produced one valid initial full-screen signature containing CPU and Memory panels. The separate Memory/ARC comparison found no `ARC:` row in either capture. Acceptance requires at least two identical valid signatures and a 1.2-second stable interval after at least 200 ms without PTY output or pending decoder bytes. Neither run reached that stable render, though no CLI error was detected. The observed equality is insufficient to determine acceptance. Load average was 99.81 before and 99.81–105.46 during the two runs. Evidence: `/tmp/ntop-wp15b-d11-diagnostic-1792900682501-9a2678/harness-output/d11-comparison.json`. |

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

# WP15b Nonfunctional and Difference Acceptance Measurements

WP15b is the local run label for these WP15 nonfunctional and difference acceptance measurements. The D1, D4, and D11 measurements used macOS 26.6.2, arm64, and an Apple M4 Max. macOS measurements used the native `ntop` executable. PTY tests used 160 columns by 48 rows except D4, which used 320 columns by 48 rows to display full User and Time values; refresh was 700 ms where applicable. The comparator was btm 0.14.9. Raw captures and JSON remain in local temporary storage and are not committed; their paths below are machine-local and may not exist elsewhere.

N3–N6 are carried over from an earlier local measurement run. Their JSON and manifest files do not record a collection date, executable digest, source commit, or harness revision, so the exact date and executable and harness versions cannot be identified. The current harness sources for reproducing those measurements are `t/e2e/wp15b_measure.py`, `t/e2e/wp15b_measure_terminal.py`, `t/e2e/wp15b_measure_support.py`, and `t/e2e/wp15b_n6.py`; their current contents do not establish which revisions produced the earlier records. D1 and D11 were rerun with a native executable rebuilt from the current workspace and revised local measurement harnesses; no ntop collector code changed for those reruns. The evidence paths below identify the reruns.

## FR-001 condition A live-grid comparison

Build the `ntop` executable and make the pinned Lisp dependencies available to SBCL. Use btm 0.14.9, then run the comparison from the worktree:

```sh
sbcl --script scripts/compare-condition-a.lisp ./ntop "$(command -v btm)" /tmp/ntop-fr001-condition-a.tsv
```

The optional positional arguments are the ntop executable, btm executable, output table, sampling interval in milliseconds, and per-process timeout in seconds. The defaults are `ntop`, `btm`, `/tmp/ntop-fr001-condition-a.tsv`, 700 ms, and 90 seconds. The command compares every dimension returned by `golden-dimensions`, writes one TSV row per dimension, and exits nonzero if any comparison differs. Captured PTY bytes are saved beside the table in `ntop-fr001-condition-a-raw/` (or the corresponding output-stem directory for a custom table path).

The runner creates the §3.4 btm configuration, uses an isolated HOME and XDG configuration directory with `TERM=xterm-256color`, and captures btm and ntop sequentially. It waits for the first-frame threshold and repeated stable criterion projections using PTY output, without a fixed sleep. The ANSI bytes are parsed into terminal grids by ntop's parser. The comparison records widget frame geometry, border cells, fixed title cells, process-header cells, and each category's foreground, background, and bold SGR attributes. CPU load numbers in the CPU title are excluded. The TSV includes a first-difference description for mismatches; raw captures are retained for inspection.

## FR-002 CPU frequency comparison

On macOS, run this from the ntop worktree:

```sh
scripts/fr002-powermetrics-compare.sh
```

Run as a non-root user on macOS with Python 3.9 or later, SBCL, Nix with `nix-command` and `flakes` enabled, `sudo`, and `powermetrics`. Nix may fetch pinned Lisp sources that are not already cached locally. The script prompts once for administrator authentication, loads ntop as the invoking user, and runs `sudo -n powermetrics --samplers cpu_power -i 1000 -n 5` alongside five ntop IOReport reads. The JSON result is written to a uniquely named file in the current directory; pass `--output PATH` to choose a different location.

The ntop frequency for each core is weighted by active IOReport residency, with DOWN and IDLE states excluded. Within each capture, the script combines active core frequencies by residency, then compares the mean of each source's active sample frequencies with powermetrics' direct cluster `HW active frequency` values. Sample indexes identify records within each capture and do not assert matching time windows. The script records the delay between powermetrics starting and releasing the ntop probe; a delay over 100 ms makes the result incomplete. Each ntop read waits 950 ms, then measures a 50 ms IOReport interval, with one read per second to follow powermetrics' 1-second sampling cadence. A single legacy `P-Cluster` value is compared with ntop's P-core groups combined; `P0-Cluster` and `P1-Cluster` are matched separately. The ntop core indexes must cover the `hw.ncpu` count or the measurement is reported as unavailable. Unmapped clusters are reported as N/A. A difference above 10% fails only when the capture is complete and classified. A missing DVFS table or unavailable reading remains N/A; the script does not substitute another frequency. A zero frequency with zero active residency is idle and excluded; zero frequency with positive active residency, missing samples, or incomplete cluster data prevents a PASS or FAIL result.

`PASS` means the measured acceptance condition was met. `FAIL` means a complete measurement exceeded its limit or disagreed with its expected value. `INCOMPLETE` means the required observation could not be established. Load values are one-minute averages unless shown as a three-value tuple in 1-, 5-, and 15-minute order. The one-minute load gate (`<40`) applies only to the time-based observations N1, N3, N4, and N5. N2 records load for context but includes every RSS sample in acceptance because unrelated CPU load has little effect on resident memory; filtering samples would break the 30-minute ceiling observation or remove the exact two- and 30-minute growth endpoints. In `paired_high_load` results, paired btm and ntop runs provide context, while ntop is evaluated against its absolute acceptance limit. An N4/N5 sample passes the load gate only when both one-minute readings immediately before and after its run are below 40. If no sample passes, the specified fallback alternates btm and ntop actions; btm results are context only. For each N4/N5 action, the sample is the raw PTY output collected after the key or resize event. N4 latency runs from immediately before sending `e` to the first PTY chunk that displays the expected layout marker. N5 latency runs from immediately before the resize request to the first PTY chunk that completes the full screen at the requested dimensions. These event-to-change and event-to-redraw times are separate from last-byte and quiet-completion times. The quiet gate is reached after at least 250 ms with no further PTY bytes following nonempty output; it is separate from confirming a visible change or complete redraw.

## Nonfunctional requirements

| ID | Requirement | Result | Conditions and measured values |
|---|---|---|---|
| N1 | CPU use versus btm | **PASS** | Five alternating btm/ntop pairs, 60 seconds per run, with 86 process-group CPU samples per run. Per-run median CPU values were 7.62–49.55% for btm (median 14.32%) and 10.16–41.81% for ntop (median 15.75%). The difference was +1.42 percentage points (+9.94% relative). The rule passes if either the relative increase is at most 10% or the difference is at most 0.5 percentage points; this result passes by the relative allowance. One-minute load before and after each run ranged from 74.95 to 153.34, so this high-load result does not establish low-load behavior. |
| N2 | RSS ceiling and growth | **PASS** | 61 RSS samples at 30-second intervals over 1,800 seconds, with display retention set to 10 minutes. One-minute load min/median/max was 79.02/108.86/143.83. RSS min/median/max was 73.88/142.11/176.36 MiB, with the maximum below the 200 MiB ceiling. At two minutes, RSS was 172.81 MiB at load 117.06; at 30 minutes, it was 73.88 MiB at load 108.67. Relative RSS change was -57.25%, calculated from the two-minute value as `((73.88 - 172.81) / 172.81) * 100`. U9 requires RSS to stay at or below 200 MiB and the 30-minute value to be no more than 5% above the two-minute value; this result meets both limits. Ring expansion followed by garbage collection could explain the decline, but this run recorded no measurements that prove that cause. |
| N3 | `--version` startup time | **PASS** | Under the paired-high-load fallback, all three ntop launches met the 100 ms limit at 50.611, 57.211, and 57.686 ms. No ntop run was load-eligible (0/3); paired btm context samples were 30.963, 12.973, and 14.120 ms. One-minute load was about 77.68. Evidence: `/tmp/ntop-wp15b-n3-182544138ba3442faca8d177c0f411f2/n3.json`. |
| N4 | Key-to-visible-response time | **PASS** | Twenty `e` actions each switched between Network and Processes layouts and passed the visible-toggle, latency, quiet, and cleanup checks. Ntop latency was 2.736/8.135/19.474 ms (min/median/max), within the 50 ms median limit. One-minute load at run boundaries was 88.36/132.81, and 0/20 samples were below 40. Under the specified paired fallback, the ntop absolute criterion passed; btm supplied context for 20/20 actions without a verified equivalent layout-switch action. Evidence: `/tmp/ntop-wp15b-n4-db00938151cc44088818261a63304229/n4.json`. |
| N5 | Resize redraw time | **PASS** | Twenty resize actions passed the redraw, latency, quiet, and cleanup checks. Ntop latency was 5.828/23.503/158.582 ms (min/median/max), within the 100 ms median limit. One-minute load at run boundaries was 141.82/119.97, and 0/20 samples were below 40. Under the specified paired fallback, the ntop absolute criterion passed; btm supplied context for 20/20 identical resize actions. Evidence: `/tmp/ntop-wp15b-n5-23f8c97f9e084b50bc7483154630de54/n5.json`. |
| N6 | Terminal-state restoration | **PASS** | All five built-ntop cases passed: normal exit (0), SIGINT (130), SIGTERM (143), SIGHUP (129), and an injected Lisp exception (1). `stty -a` and independent termios snapshots matched before and after every case. The harness observed a DEC private-mode reset and modeled the cursor-status query reply from ntop's captured terminal sequences; this was not an independent terminal response. All recorded target and wrapper PIDs were absent after the run. Load average (1/5/15 minutes) was 83.993/99.893/105.150 before the run and ranged from 83.625–109.852/98.931–102.992/103.548–105.362 during it. Evidence: `/tmp/ntop-wp15b-n6-4s4fcec6/manifest.json`. |

For N1, all five btm runs reported `EPERM` when cleanup attempted to signal the owned process group. Closing the owned PTY then allowed the child to exit; each PID and process group was confirmed absent and reaped. This cleanup warning did not invalidate the recorded CPU samples.

The N6 btm normal, SIGINT, SIGTERM, and SIGHUP references timed out before reaching raw mode. The SBCL source-runtime Lisp-exception reference timed out waiting for raw-mode readiness. These reference cases are outside the acceptance count. The SIGKILL case is also outside acceptance. Its `diagnostic_status: PASS` means the harness and SIGKILL-specific checks passed; it is not a terminal-restoration pass because restoration checks are not evaluated for SIGKILL. Raw mode remained active, as expected when the process cannot run cleanup.

## [Difference acceptance tests](differences.md)

| ID | Difference | Result | Conditions and measured values |
|---|---|---|---|
| D1 | macOS process write rate | **PASS** | The writer used Darwin `F_NOCACHE`, `fsync` after each 100,000-byte chunk, and 100 ms absolute pacing. It wrote 19,700,000 bytes in 19.705827 s (999,704.30 B/s); file size matched the child byte count. Of 26 PTY observations, 20 had valid W/s values; those 20 values form the acceptance set. Their displayed median was 1,000,000 B/s (min/max 811,000/1,000,000); 18/20 individual observations were within 900,000–1,100,000 B/s, and two were 811,000 B/s. The existing D1 gate passes on 20 valid observations and a median within ±10%; the two out-of-range samples remain visible here. Independent Darwin `proc_pid_rusage` V2 reads succeeded and matched all 26 observation timestamps and displayed samples; the internal trace contains 14 cumulative samples. One-minute load during observations ranged from 54.82 to 57.78; 1/5/15-minute load was 52.75/55.04/66.57 before and 54.82/55.48/66.47 after. Source review found that btm 0.14.9 labels the per-refresh byte delta as `/s` without dividing by actual elapsed time, so its previous 0/8,000,000 B/s output is context, not a normalized rate comparator ([btm process collector](https://github.com/ClementTsang/bottom/blob/0.14.9/src/collection/processes/unix/process_ext.rs#L115-L141), [sysinfo macOS counter](https://github.com/GuillaumeGomez/sysinfo/blob/v0.39.6/src/unix/apple/macos/process.rs#L185-L192), [btm unit conversion](https://github.com/ClementTsang/bottom/blob/0.14.9/src/utils/conversion.rs#L20-L30)). Evidence: `/tmp/ntop-wp15b-d1-20261025-secondgen/d1.json`, `/tmp/ntop-wp15b-d1-20261025-secondgen/ntop-internal-samples.tsv`, and `/tmp/ntop-wp15b-d1-20261025-secondgen/writer-io-samples.tsv`. |
| D2 | Linux fractional I/O rate | **PASS** | One Linux fixture test used a 0.7-second interval and a write-byte delta of 7,168 bytes, producing the expected nonzero rate of 10,240 B/s. |
| D4 | WindowServer User and start time | **PASS** | Across three measurements on a 320-by-48 PTY at 700 ms refresh, the single visible WindowServer row's User matched `ps -o user=,lstart=` exactly (3/3). Its Time-derived start differed from `lstart` by 58 s, 42 s, and 29 s; all were within 62 s. The allowance is `ceil(60 s Time display precision + 0.7 s UI refresh interval + 1 s ps lstart resolution)`. |
| D5 | Unavailable process values | **PASS** | A privileged process displayed `N/A` for Mem%. |
| D6 | APFS volume usage | **PASS** | The system volume displayed 13 GB used. `df -k` reported 12,340,992 KiB used both before and after, consistent with the rounded display. |
| D7 | Named temperature sensors | **PASS** | All 45 named sensor/service records had a corresponding temperature row (45/45); duplicate names used numbered suffixes. |
| D8 | First CPU average | **PASS** | Sixteen workers saturated all 16 logical cores for 2.0015 seconds; per-core utilization was 100.0/100.0/100.0% (min/median/max). Workers were confirmed stopped before ntop launched, 25.8 ms after the load window ended. The first displayed CPU average was 97% at 3.1445 seconds after launch, meeting the requirement of at least 90% after the two-second all-core load. The D8 harness exited 1 because its first cleanup check did not confirm the app PID; a later exact PID check found it absent, and all workers were confirmed stopped before app launch. |
| D9 | Fixed sampling period | **PASS** | The application-sampler test used a 700 ms fixed period and a simulated 300 ms collection delay. Three live samples remained exactly 700 ms apart on the virtual clock (0 ms period error); the warm-up sample was discarded. |
| D11 | `--free_arc` behavior | **PASS** | The test `keeps default and expanded Memory fixture screens unchanged by --free_arc` in `t/cli/presentation-screen-cli-test.lisp` runs the CLI with and without the flag, passes the parsed config to the fixed screen fixture renderer in `t/app/presentation-screen-test.lisp`, and compares every cell's character and style at 80×24 and 160×48 in the default and expanded Memory layouts. No cells are excluded. |

### D1 raw counter trace

Each row contains the writer PID's cumulative read and write bytes at the attached monotonic sample time. The interval rate is `(current write bytes − previous write bytes) / elapsed seconds`. The raw writer trace records each 100,000-byte chunk and its `fsync` timing in `/tmp/ntop-wp15b-d1-20261025-secondgen/writer-io-samples.tsv`.

| Sample | Monotonic timestamp (ns) | Cumulative read bytes | Cumulative write bytes | Elapsed (s) | Write delta (bytes) | Derived rate (B/s) |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 378467596208000 | 49,152 | 532,480 | — | — | — |
| 2 | 378469032074000 | 225,280 | 2,007,040 | 1.435866 | 1,474,560 | 1,026,948 |
| 3 | 378470317792000 | 438,272 | 3,530,752 | 1.285718 | 1,523,712 | 1,185,106 |
| 4 | 378471635713000 | 684,032 | 5,152,768 | 1.317921 | 1,622,016 | 1,230,738 |
| 5 | 378472902298000 | 897,024 | 6,676,480 | 1.266585 | 1,523,712 | 1,203,008 |
| 6 | 378474146477000 | 1,093,632 | 8,069,120 | 1.244179 | 1,392,640 | 1,119,324 |
| 7 | 378475269085000 | 1,306,624 | 9,576,448 | 1.122608 | 1,507,328 | 1,342,702 |
| 8 | 378476485262000 | 1,486,848 | 10,854,400 | 1.216177 | 1,277,952 | 1,050,794 |
| 9 | 378478202074000 | 1,683,456 | 12,247,040 | 1.716812 | 1,392,640 | 811,178 |
| 10 | 378479632575000 | 1,961,984 | 14,229,504 | 1.430501 | 1,982,464 | 1,385,853 |
| 11 | 378480959867000 | 2,207,744 | 15,982,592 | 1.327292 | 1,753,088 | 1,320,801 |
| 12 | 378482258194000 | 2,420,736 | 17,489,920 | 1.298327 | 1,507,328 | 1,160,977 |
| 13 | 378483621858000 | 2,633,728 | 18,997,248 | 1.363664 | 1,507,328 | 1,105,351 |
| 14 | 378485267377000 | 2,879,488 | 20,750,336 | 1.645519 | 1,753,088 | 1,065,371 |

## WP13 CLI invalid-input parity

The focused comparison used the built `ntop` executable and btm 0.14.9 on macOS. The 90 unique cases include 74 generated invalid inputs and 15 explicit checks, including seven accepted TUI invocations; some arguments appear in multiple groups.

All 90 exit statuses were observed and matched, with no unexpected results. For the 83 captured cases, stdout/stderr presence also matched. For the seven accepted TUI cases, both programs rendered visible screen content through the existing PTY helper; the runner waited at least 3.5 seconds after launch, confirmed a nonempty terminal screen, then sent `q` and observed exit status 0. The PTY combines terminal output, so stdout/stderr placement is not applicable to TUI cases. Captured stream checks compare only whether each stream is nonempty; diagnostic text is not compared.

Reproduce it with `python3 t/e2e/wp13_cli_parity.py --ntop ./ntop --btm "$(command -v btm)" --output /tmp/wp13-cli` after building `./ntop` and selecting btm 0.14.9. The `--output` value is a filename stem; the script appends `.json` and `.md`.

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

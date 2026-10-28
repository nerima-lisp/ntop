# WP15b Nonfunctional and Difference Acceptance Measurements

WP15b is the local run label for these WP15 nonfunctional and difference acceptance measurements. The release process will attach the benchmark evidence as `ntop-v0.1.0-benchmark-evidence.tar.gz`; paths below are relative to the archive root. The D1, D4, and D11 measurements used macOS 26.6.2, arm64, and an Apple M4 Max. macOS measurements used the native `ntop` executable. PTY tests used 160 columns by 48 rows except D4, which used 320 columns by 48 rows to display full User and Time values; refresh was 700 ms where applicable. The comparator was btm 0.14.9. Local evidence copies remain under `.mediator/benchmark-evidence/` and are not committed.

N3 and N5 are carried over from an earlier local measurement run. Their JSON files do not record a collection date, executable digest, source commit, or harness revision, so their exact provenance cannot be identified. The current scripts, `t/e2e/wp15b_measure.py` and `t/e2e/wp15b_measure_terminal.py`, provide reproduction references but do not identify the revisions used for those earlier measurements. D1 was rerun with a local native build and a revised measurement harness; no ntop collector code changed for that rerun. D11 was rerun using the fixture test described below. N6 was rerun against a locally built executable; its evidence distinguishes the five built-ntop acceptance cases from the source-runtime reference. These local rerun records do not identify a source commit or exact harness revision.

## FR-001 condition A live-grid comparison

Build the `ntop` executable and make the pinned Lisp dependencies available to SBCL. Use btm 0.14.9, then run the comparison from the worktree:

```sh
mkdir -p .mediator/benchmark-evidence
export CL_SOURCE_REGISTRY="$(python3 -c 'import json, subprocess; print(json.loads(subprocess.check_output(["python3", "scripts/darwin-workflow.py", "resolve", "--emit-environment-json"], text=True))["CL_SOURCE_REGISTRY"])')"
sbcl \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd #P"./ntop.asd")' \
  --eval '(asdf:load-system "ntop/e2e")' \
  --script scripts/compare-condition-a.lisp \
  ./ntop "$(command -v btm)" \
  .mediator/benchmark-evidence/ntop-fr001-condition-a-repro.tsv
```

The environment command resolves the pinned Nix dependency sources for ASDF. The `--eval` arguments load `ntop/e2e` before SBCL reads the script, so its package-qualified call is available. The optional positional arguments are the ntop executable, btm executable, output table, sampling interval in milliseconds, and per-process timeout in seconds. The script defaults to `ntop`, `btm`, a temporary output table, 700 ms, and 90 seconds. It compares every dimension returned by `golden-dimensions`, writes one TSV row per dimension, and exits nonzero if any comparison differs. Captured PTY bytes are saved beside the table in a raw directory named from the output stem.

The runner writes a btm configuration with three rows at 1:1:2 height ratios: CPU and memory; network, disk, and temperature; and processes. It uses an isolated HOME and XDG configuration directory with `TERM=xterm-256color`, then captures btm and ntop sequentially. It samples PTY output every 300 ms, waits at least three refresh intervals, and requires three consecutive identical comparisons before accepting a capture. Each comparison includes the terminal-grid size and widget frame geometry, plus border and fixed-title cells and the foreground, background, and bold attributes of border, title, and process-header cells. Process-header text is used to locate header attributes but is not compared; CPU load values in the CPU title are excluded. A process frame spanning more than three rows is ready only after process-header cells appear; shorter frames may proceed without a visible header. The ANSI bytes are parsed into terminal grids by ntop's parser. The TSV includes a first-difference description for mismatches; raw captures are retained for inspection.

Result: **INCOMPLETE**. The retry started at 09:19 JST on 2026-10-28 and exited 1 after the first 40×12 condition-A capture timed out at 90.004 seconds. The last PTY output arrived 89.998 seconds after capture began, and no stable signature was obtained. No dimensions were compared and no TSV was created; the timeout was unchanged. A separate reference capture exposed a capture-engine stability issue, and a fix for that issue is pending. Whether it caused this timeout is unknown; no further condition-A attempt was made pending the fix. Evidence in `ntop-v0.1.0-benchmark-evidence.tar.gz`: `ntop-fr001-condition-a-repro-result.json` and `ntop-fr001-condition-a-repro-raw/`.

## FR-002 CPU frequency comparison

On macOS, run this from the ntop worktree:

```sh
scripts/fr002-powermetrics-compare.sh
```

Run as a non-root user on macOS with Python 3.9 or later, SBCL, Nix with `nix-command` and `flakes` enabled, `sudo`, and `powermetrics`. Nix may fetch pinned Lisp sources that are not already cached locally. The script prompts once for administrator authentication, loads ntop as the invoking user, and runs `sudo -n powermetrics --samplers cpu_power -i 1000 -n 5` alongside five ntop IOReport reads. The JSON result is written to a uniquely named file in the current directory; pass `--output PATH` to choose a different location.

The ntop frequency for each core is weighted by active IOReport residency, with DOWN and IDLE states excluded. Within each capture, the script combines active core frequencies by residency, then compares the mean of each source's active sample frequencies with powermetrics' direct cluster `HW active frequency` values. Sample indexes identify records within each capture and do not assert matching time windows. The script records the delay between powermetrics starting and releasing the ntop probe; a delay over 100 ms makes the result incomplete. Each ntop read waits 950 ms, then measures a 50 ms IOReport interval, with one read per second to follow powermetrics' 1-second sampling cadence. A single legacy `P-Cluster` value is compared with ntop's P-core groups combined; `P0-Cluster` and `P1-Cluster` are matched separately. The ntop core indexes must cover the `hw.ncpu` count or the measurement is reported as unavailable. Unmapped clusters are reported as N/A. A difference above 10% fails only when the capture is complete and classified. A missing DVFS table or unavailable reading remains N/A; the script does not substitute another frequency. A zero frequency with zero active residency is idle and excluded; zero frequency with positive active residency, missing samples, or incomplete cluster data prevents a PASS or FAIL result.

This page documents the FR-002 procedure; it does not report an FR-002 measurement result.

`PASS` means the measured acceptance condition was met. `FAIL` means a complete measurement exceeded its limit or disagreed with its expected value. `INCOMPLETE` means the required observation could not be established. Load values are one-minute averages unless shown as a three-value tuple in 1-, 5-, and 15-minute order. N3 and N4 use a load limit below 40; N5 uses a limit at or below 16. N1 and N2 record load for context. N2 includes every RSS sample in acceptance because unrelated CPU load has little effect on resident memory; filtering samples would break the 30-minute ceiling observation or remove the exact two- and 30-minute growth endpoints. In load-gated N4 and N5 runs, a sample qualifies only when both one-minute readings immediately before and after the action meet that requirement. Passing `--btm` selects paired high-load mode for N4: btm and ntop are run once per sample in alternating order, and each process is closed and reaped before the next starts. btm is context only because the harness has not verified an equivalent btm action for ntop's layout toggle. N5 does not use paired high-load mode. For each N4/N5 action, the sample is the raw PTY output collected after the key or resize event. N4 latency runs from immediately before sending `e` to the first PTY chunk that displays the expected layout marker. N5 latency runs from immediately before the resize request to the first PTY chunk that completes the full screen at the requested dimensions. These event-to-change and event-to-redraw times are separate from last-byte and quiet-completion times. The quiet gate is reached after at least 250 ms with no further PTY bytes following nonempty output; it is separate from confirming a visible change or complete redraw.

## Nonfunctional requirements

| ID | Requirement | Result | Conditions and measured values |
|---|---|---|---|
| N1 | CPU use versus btm | **PASS** | Five alternating btm/ntop pairs, 60 seconds per run, with 86 process-group CPU samples per run. Per-run median CPU values were 7.62–49.55% for btm (median 14.32%) and 10.16–41.81% for ntop (median 15.75%). The difference was +1.42 percentage points (+9.94% relative). The rule passes if either the relative increase is at most 10% or the difference is at most 0.5 percentage points; this result passes by the relative allowance. One-minute load before and after each run ranged from 74.95 to 153.34, so this high-load result does not establish low-load behavior. |
| N2 | RSS ceiling and growth | **PASS** | 61 RSS samples at 30-second intervals over 1,800 seconds, with display retention set to 10 minutes. One-minute load min/median/max was 79.02/108.86/143.83. RSS min/median/max was 73.88/142.11/176.36 MiB, with the maximum below the 200 MiB ceiling. At two minutes, RSS was 172.81 MiB at load 117.06; at 30 minutes, it was 73.88 MiB at load 108.67. Relative RSS change was -57.25%, calculated from the two-minute value as `((73.88 - 172.81) / 172.81) * 100`. Acceptance required RSS to remain at or below 200 MiB and the 30-minute value to be no more than 5% above the two-minute value; this result met both limits. Ring expansion followed by garbage collection could explain the decline, but this run recorded no measurements that prove that cause. |
| N3 | `--version` startup time | **PASS** | The product `ntop` executable built by `python3 scripts/darwin-workflow.py build` completed three `--version` runs in 62.613, 42.493, and 37.854 ms (median 42.493 ms); all exited 0, met the 100 ms limit, and had before/after load1 of 16.00. A separate marker-instrumented profile build, which is not the product executable, measured process launch to init-hook at 45.563, 37.664, and 42.483 ms (median 42.483 ms), then init-hook to exit-hook at 0.750, 0.768, and 0.846 ms (median 0.768 ms). Evidence in `ntop-v0.1.0-benchmark-evidence.tar.gz`: `ntop-wp15b-n3-product-vmd__bna/n3-product.json` and `ntop-wp15b-n3-5w0w7a8y/n3.json`. |
| N4 | Key-to-visible-response time | **PASS** | The previous capture cannot be judged by the repository harness: it searched for `Network` at fixed row 16, while saved 48×160 screens show it at row 12 in the default layout and the expanded Processes frame at row 0 (the default Processes panel is at row 24). A post-run replay found 20/20 visible toggles and all 20 samples within the before/after load1 limit; that replay is diagnostic only and does not establish acceptance. The corrected harness derives marker position from screen frame titles and layout geometry. In the new 20-sample run, every sample had before/after load1 below 40 (range 16.07–31.99); run-boundary load1 was 16.07/28.17. All 20 layout changes and quiet checks passed, and cleanup was verified. The derived marker rows were 12 for the default Network layout and 0 for expanded Processes. Visible-change latency was 11.361/20.259/62.641 ms (min/median/max); the median met the 50 ms limit. Evidence: `ntop-v0.1.0-benchmark-evidence.tar.gz` → `ntop-wp15b-n4-2usb3aua/n4.json`. |
| N5 | Resize redraw time | **PASS** | Twenty resize actions completed full redraws and passed visual, quiet, and cleanup checks. Full-redraw latency was 9.604/20.598/63.301 ms (min/median/max); all 20 were below 100 ms. All 20 samples met the before/after load1 limit of 16 (sample readings ranged from 10.86 to 14.50); run-boundary load1 was 14.98/10.87. Evidence in `ntop-v0.1.0-benchmark-evidence.tar.gz`: `ntop-wp15b-n5-bklq3lvo/n5.json`. |
| N6 | Terminal-state restoration | **PASS** | All five built-ntop acceptance cases passed: normal exit, SIGINT, SIGTERM, SIGHUP, and an injected Lisp exception (`NTOP_N6_TEST_EXCEPTION=raise-on-first-render`). The source-runtime SBCL exception is a separate passing reference case and is not included in the acceptance count. The wrapper queried cursor position with DSR 6n (`1b 5b 36 6e`) after the target exited while it still owned the PTY; the helper received `1b 5b 34 38 3b 31 52`, matching the pyte peer response. A separate DECRQM query checked mode 25, which controls cursor visibility (`1b 5b 3f 32 35 24 70`); the response `1b 5b 3f 32 35 3b 31 24 79` reported that mode as set. `stty -a` and independent termios snapshots were unchanged in all five cases. The DSR probe is wrapper-generated, not output from ntop; pyte is a terminal emulator peer. Evidence: `ntop-v0.1.0-benchmark-evidence.tar.gz` → `ntop-wp15b-n6-5avtmuzo/manifest.json` |

For N1, all five btm runs reported `EPERM` when cleanup attempted to signal the owned process group. Closing the owned PTY then allowed the child to exit; each PID and process group was confirmed absent and reaped. This cleanup warning did not invalidate the recorded CPU samples.

The N6 btm normal, SIGINT, SIGTERM, and SIGHUP references timed out before reaching raw mode. The SBCL source-runtime Lisp-exception reference timed out waiting for raw-mode readiness. These reference cases are outside the acceptance count. The SIGKILL case is also outside acceptance. Its `diagnostic_status: PASS` means the harness and SIGKILL-specific checks passed; it is not a terminal-restoration pass because restoration checks are not evaluated for SIGKILL. Raw mode remained active, as expected when the process cannot run cleanup.

## [Difference acceptance tests](differences.md)

| ID | Difference | Result | Conditions and measured values |
|---|---|---|---|
| D1 | macOS process write rate | **FAIL** | The writer used Darwin `F_NOCACHE`, `fsync` after each 100,000-byte chunk, and 100 ms absolute pacing. It wrote 16,800,000 bytes in 16.800664 s (999,960.46 B/s); file size matched the child byte count. All 20 observations had valid W/s values and met the before/after load1 limit of 16 (range 8.86–9.56). Displayed rate had a median of 1,000,000 B/s (min/max 618,000/2,000,000); 18/20 samples were within 900,000–1,100,000 B/s, so the per-sample ±10% criterion failed. Observations 12 and 13 displayed `618KB/s` and `2MB/s`; both strings are present in the raw ANSI capture. The internal process-counter trace reproduces approximately 618,228 B/s and 1,736,319 B/s for their corresponding intervals. The btm 0.14.9 cross-check with the same 1,000,000 B/s writer setup completed 20/20 valid, low-load observations (started at load1 14.61); 17/20 displayed values were within ±10%, with three `2MB/s` outliers. Both tools had outlier samples consistent with variability in Darwin process-write counters, but these measurements do not establish the cause. btm 0.14.9 formats process W/s using zero decimal places below 1 GB/s; this maps 1,736,319 B/s to `2MB/s`, matching the ntop capture ([formatter source](https://github.com/ClementTsang/bottom/blob/0.14.9/src/utils/conversion.rs#L27-L39), [process W/s column](https://github.com/ClementTsang/bottom/blob/0.14.9/src/widgets/process_table/process_data.rs#L329-L332)). Writer, file-match, counter-boundary, internal-trace, and 20/20 Darwin `proc_pid_rusage` checks passed for ntop. The btm run also passed writer-rate, file-match, and rusage diagnostics; its per-sample criterion failed. Evidence in `ntop-v0.1.0-benchmark-evidence.tar.gz`: `ntop-wp15b-d1-g3xotgj0/d1.json`, `ntop-wp15b-d1-g3xotgj0/ntop.ansi`, `ntop-wp15b-d1-g3xotgj0/ntop-internal-samples.tsv`, `ntop-wp15b-d1-g3xotgj0/writer-write-samples.tsv`, `ntop-wp15b-d1-btm-b11f7547ce68/d1.json`, and `ntop-wp15b-d1-btm-b11f7547ce68/btm.ansi`. |
| D2 | Linux fractional I/O rate | **PASS** | One Linux fixture test used a 0.7-second interval and a write-byte delta of 7,168 bytes, producing the expected nonzero rate of 10,240 B/s. |
| D4 | WindowServer User and start time | **PASS** | Across three measurements on a 320-by-48 PTY at 700 ms refresh, the single visible WindowServer row's User matched `ps -o user=,lstart=` exactly (3/3). Its Time-derived start differed from `lstart` by 58 s, 42 s, and 29 s; all were within 62 s. The allowance is `ceil(60 s Time display precision + 0.7 s UI refresh interval + 1 s ps lstart resolution)`. |
| D5 | Unavailable process values | **PASS** | A privileged process displayed `N/A` for Mem%. |
| D6 | APFS volume usage | **PASS** | The system volume displayed 13 GB used. `df -k` reported 12,340,992 KiB used both before and after, consistent with the rounded display. |
| D7 | Named temperature sensors | **PASS** | All 45 named sensor/service records had a corresponding temperature row (45/45); duplicate names used numbered suffixes. |
| D8 | First CPU average | **PASS** | Sixteen workers saturated all 16 logical cores for 2.0015 seconds; per-core utilization was 100.0/100.0/100.0% (min/median/max). Workers were confirmed stopped before ntop launched, 25.8 ms after the load window ended. The first displayed CPU average was 97% at 3.1445 seconds after launch, meeting the requirement of at least 90% after the two-second all-core load. The D8 harness exited 1 because its first cleanup check did not confirm the app PID; a later exact PID check found it absent, and all workers were confirmed stopped before app launch. |
| D9 | Fixed sampling period | **PASS** | The application-sampler test used a 700 ms fixed period and a simulated 300 ms collection delay. Three live samples remained exactly 700 ms apart on the virtual clock (0 ms period error); the warm-up sample was discarded. |
| D11 | `--free_arc` behavior | **PASS** | The test `keeps default and expanded Memory fixture screens unchanged by --free_arc` in `t/cli/presentation-screen-cli-test.lisp` runs the CLI with and without the flag, passes the parsed config to the fixed screen fixture renderer in `t/app/presentation-screen-test.lisp`, and compares every cell's character and style at 80×24 and 160×48 in the default and expanded Memory layouts. No cells are excluded. |

### D1 raw counter trace

The copied D1 process-counter trace records the writer PID's cumulative read and write bytes at each monotonic sample time. The interval rate is `(current write bytes − previous write bytes) / elapsed seconds`. Rows 12–14 below show the two counter intervals corresponding to the displayed outliers. Per-write and `fsync` timings are in `ntop-wp15b-d1-g3xotgj0/writer-write-samples.tsv` and `ntop-wp15b-d1-g3xotgj0/writer-io-samples.tsv` inside `ntop-v0.1.0-benchmark-evidence.tar.gz`.

| Sample | Monotonic timestamp (ns) | Cumulative read bytes | Cumulative write bytes | Elapsed (s) | Write delta (bytes) | Derived rate (B/s) |
|---:|---:|---:|---:|---:|---:|---:|
| 12 | 33358227335000 | 1,216,512 | 9,003,008 | 0.686268 | 819,200 | 1,193,703 |
| 13 | 33359525911000 | 1,331,200 | 9,805,824 | 1.298576 | 802,816 | 618,228 |
| 14 | 33360195871000 | 1,495,040 | 10,969,088 | 0.669960 | 1,163,264 | 1,736,319 |

## WP13 CLI parity

The focused rerun used the built `ntop` executable and btm 0.14.9 on macOS and completed 94 unique cases: 74 generated invalid inputs, 15 explicit checks (one overlaps a generated input), two additional valid TUI invocations, and four configuration-path behavior cases.

All 90 parity cases had matching exit statuses. The four configuration-path cases produced the expected difference: `ntop` rejected the path with status 2, while btm entered its TUI and exited with status 0. All 94 cases met their per-case expectations, with no unexpected results. For the 83 captured cases, stdout/stderr presence matched. In the seven accepted TUI cases, both programs rendered visible screen content through the existing PTY helper; the runner waited at least 3.5 seconds after launch, confirmed a nonempty terminal screen, then sent `q` and observed exit status 0. In the four configuration-path cases, the helper confirmed visible btm screens before sending `q`. The PTY combines terminal output, so stdout/stderr placement is not applicable to TUI cases. Captured stream checks compare only whether each stream is nonempty; diagnostic text is not compared.

Reproduce it with `python3 t/e2e/wp13_cli_parity.py --ntop ./ntop --btm "$(command -v btm)" --output .mediator/benchmark-evidence/wp13-cli` after building `./ntop` and selecting btm 0.14.9. The `--output` value is a filename stem; the script appends `.json` and `.md`. The rerun reports `status: success`, 94 completed and passed cases, four expected status differences, and zero unexpected cases. Evidence in `ntop-v0.1.0-benchmark-evidence.tar.gz`: `wp13-cli.json` and `wp13-cli.md`.

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

The four configuration-path cases are outside the CLI parity specification. ntop rejects these values before launching its TUI, while btm proceeds to terminal initialization. These expected differences are included in the 94-case run but are not counted as parity failures.

| Arguments | Expected status (ntop / btm) | Observed status (ntop / btm) | TUI screen |
|---|---:|---:|---|
| `["--config_location", "/dev/null"]` | 2 / 0 | 2 / 0 | btm |
| `["--config-location", "/dev/null"]` | 2 / 0 | 2 / 0 | btm |
| `["--config", "/dev/null"]` | 2 / 0 | 2 / 0 | btm |
| `["-C", "/dev/null"]` | 2 / 0 | 2 / 0 | btm |

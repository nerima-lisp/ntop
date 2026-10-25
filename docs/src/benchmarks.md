# WP15b Nonfunctional and Difference Acceptance Measurements

WP15b is the local run label for these WP15 nonfunctional and difference acceptance measurements. Measurements were collected on 2026-10-25 with macOS 26.6.2, arm64, and an Apple M4 Max. The native `ntop` executable was used for the macOS runs. PTY tests used 160 columns by 48 rows and a 700 ms refresh rate where applicable. The comparator was btm 0.14.9. Raw captures and JSON remain in local temporary storage and are not committed; their paths below are machine-local and may not exist elsewhere.

The N3–N6 rows retain their previous executable and harness records. D1 and D11 were rerun with a native executable rebuilt from the collector source in this change and the revised local harnesses. Raw captures and JSON remain in local temporary storage; the evidence paths below identify those reruns.

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
| D1 | macOS process write rate | **FAIL** | Ntop's 20 screen observations showed `0` B/s 14 times, `2,000,000` B/s 3 times, and `5,000,000` B/s 3 times; 0/20 were within 900,000–1,100,000 B/s. The writer sustained 957,222.97 B/s and wrote 23.5 MB, matching the file size. The internal trace recorded 10 cumulative process-counter samples 1.68–3.51 seconds apart. The counter remained flat between large increments; independent `proc_pid_rusage` reads showed the same coarse updates. The observed rates match Δbytes divided by the recorded monotonic sample-time deltas; no alternate-clock calculation was found. The btm 0.14.9 reference showed `0` B/s 15 times and `8,000,000` B/s 5 times, with a 931,457.12 B/s writer; neither implementation met the absolute limit. D1 requires 1 MB/s ±10%, not parity with btm. One-minute load was 80.78–81.51 during ntop observations. Evidence: `/tmp/ntop-wp15b-d1-28263-1792912902781869000/d1.json` and `/tmp/ntop-wp15b-d1-btm-20261025-04/d1.json`. |
| D2 | Linux fractional I/O rate | **PASS** | One Linux fixture test used a 0.7-second interval and a write-byte delta of 7,168 bytes, producing the expected nonzero rate of 10,240 B/s. |
| D4 | User and Time for other-user processes | **PASS** | With a 160-by-48 PTY and 700 ms refresh, 4 visible other-user rows matched `ps -o pid=,ruser=,etime= -A`: User 4/4 exactly and Time 4/4 within 3 seconds. The comparator found 268 other-user processes; 13 process rows were visible. The screen was captured after 15.62 seconds. The 3-second allowance covers the UI refresh and comparator scan delay. PTY cleanup required SIGTERM and SIGKILL; the `ntop` child was reaped. |
| D5 | Unavailable process values | **PASS** | A privileged process displayed `N/A` for Mem%. |
| D6 | APFS volume usage | **PASS** | The system volume displayed 13 GB used. `df -k` reported 12,340,992 KiB used both before and after, consistent with the rounded display. |
| D7 | Named temperature sensors | **PASS** | All 45 named sensor/service records had a corresponding temperature row (45/45); duplicate names used numbered suffixes. |
| D8 | First CPU average | **PASS** | Sixteen workers saturated all 16 logical cores for 2.0015 seconds; per-core utilization was 100.0/100.0/100.0% (min/median/max). ntop started 25.8 ms after the load window ended; the first displayed CPU average was 97% at 3.1445 seconds after launch. The acceptance condition is the first displayed average under the stated load window. The command returned 1 because its cleanup check initially did not confirm the app PID; a later exact PID check found it absent, and all workers were confirmed stopped before app launch. |
| D9 | Fixed sampling period | **PASS** | The application-sampler test used a 700 ms fixed period and a simulated 300 ms collection delay. Three live samples remained exactly 700 ms apart on the virtual clock (0 ms period error); the warm-up sample was discarded. |
| D11 | `--free_arc` behavior | **FAIL** | Twenty ordered pairs were captured. `--free_arc` was accepted in 20/20 flagged runs, no CLI errors appeared, and all 40 child processes were reaped and absent. The dynamic-value-excluded initial-screen signatures matched in 19/20 pairs. Pair 8 differed: the flagged capture included a RAM graph/value label that was absent in its unflagged capture. The measurement therefore does not establish unchanged display. The signature compares the 48×160 grid, CPU/Memory title positions, box corners, and static labels; it does not search for an ARC row. One-minute load was 64.72 before and 78.60 after the run (five-minute: 83.32/82.87; fifteen-minute: 81.80/81.97). Evidence: `/tmp/ntop-wp15b-d11-final-k3jl5t23/d11-comparison.json`. |

### D1 raw counter trace

The ntop run wrote all 10 internal samples to `/tmp/ntop-wp15b-d1-28263-1792912902781869000/ntop-internal-samples.tsv`. Each timestamp is the monotonic time attached to the process sample; each row is the writer PID's cumulative read and write counter. The interval rate is `(current write bytes − previous write bytes) / elapsed seconds`.

| Sample | Monotonic timestamp (ns) | Cumulative read bytes | Cumulative write bytes | Elapsed (s) | Write delta (bytes) | Derived rate (B/s) |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 369490648867000 | 0 | 4,096 | — | — | — |
| 2 | 369494163198000 | 0 | 4,096 | 3.514331 | 0 | 0 |
| 3 | 369497022218000 | 0 | 4,096 | 2.859020 | 0 | 0 |
| 4 | 369499172365000 | 12,288 | 5,246,976 | 2.150147 | 5,242,880 | 2,438,382 |
| 5 | 369501350876000 | 12,288 | 5,246,976 | 2.178511 | 0 | 0 |
| 6 | 369503252488000 | 12,288 | 5,246,976 | 1.901612 | 0 | 0 |
| 7 | 369505298053000 | 12,288 | 5,246,976 | 2.045565 | 0 | 0 |
| 8 | 369506980687000 | 12,288 | 13,635,584 | 1.682634 | 8,388,608 | 4,985,403 |
| 9 | 369508990514000 | 12,288 | 13,635,584 | 2.009827 | 0 | 0 |
| 10 | 369511264528000 | 12,288 | 13,635,584 | 2.274014 | 0 | 0 |

The independently sampled Darwin `proc_pid_rusage` V2 counter stayed at 5,246,976 bytes for screen observations 1–11, then increased by 8,388,608 bytes at observation 12 and stayed there through observation 20. This confirms coarse counter updates during the run. Ntop's screen showed the corresponding 0/2,000,000/5,000,000 B/s values while reusing each process sample across 700 ms display observations. The counter-derived rates match the recorded sample timestamps; the writer's 957,222.97 B/s file rate is not a direct measure of these delayed disk-I/O counter updates. The btm reference had the same writer and target rate but also showed 0/8,000,000 B/s pulses, so its output does not satisfy D1's absolute criterion.

## WP13 CLI parity

The focused comparison plan contains 91 unique cases, including nine generated valid samples, 74 generated invalid inputs, and 16 explicit checks. These groups overlap; seven of the valid samples also appear as explicit checks. Of the 91 cases, 83 use captured output and eight require terminal interaction: seven ordinary TUI invocations and one default-config creation. The config case gives ntop and btm independent temporary paths, then compares each created file byte-for-byte with the bundled default TOML and checks its mode against `0666 & ~umask`.

The recorded focused run used the rebuilt `./ntop` and btm 0.14.9. All 91 cases passed: exit statuses and stdout/stderr presence matched for the 83 captured cases, and no config-file mismatches, unexpected cases, or unobserved statuses remained. Both programs rendered a nonempty screen in all eight terminal-interaction cases. The config case created a 15,946-byte default TOML with SHA-256 `a079bd2f3bab33ca20079e2b159a9ad85cfec32feb4abed5c4f053f83d8b13e9` and matching mode `0600`. The PTY combines terminal output, so stdout/stderr placement is not applicable to TUI-like cases. Captured stream checks compare only whether each stream is nonempty; diagnostic text is not compared.

For an existing unreadable but writable file, ntop preserves the existing bytes and starts with built-in defaults. btm may replace such a file while creating its defaults. The added parity case covers a missing path; it does not compare this existing-file side effect.

Build `./ntop` with `python3 scripts/darwin-workflow.py build` and use btm 0.14.9. Then reproduce the comparison with `python3 t/e2e/wp13_cli_parity.py --ntop ./ntop --btm "$(command -v btm)" --output /tmp/wp13-cli`.

`Draw` names which programs rendered a visible screen (`both`, `ntop`, `btm`, or `none`). `Channels` reports whether stdout and stderr presence matched (`match`, `mismatch`, or `n/a`). `Config` reports whether created files matched byte-for-byte and by mode (`match` or `mismatch`), or `n/a` when a case did not create one.

### Case-by-case results

| Case | Arguments | Expected | ntop | btm | Draw | Channels | Config | Result |
|---|---|---:|---:|---:|---|---|---|---|
| 001 | `["--config_location", "<temporary-config-path>"]` | 0 | 0 | 0 | both | n/a | match | pass |
| 002 | `["--default_widget_count", "2"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 003 | `["--default_widget_type", "cpu"]` | 0 | 0 | 0 | both | n/a | n/a | pass |
| 004 | `["--rate", "251ms"]` | 0 | 0 | 0 | both | n/a | n/a | pass |
| 005 | `["--hide_k_threads"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 006 | `["--free_arc"]` | 0 | 0 | 0 | both | n/a | n/a | pass |
| 007 | `["--battery"]` | 0 | 0 | 0 | both | n/a | n/a | pass |
| 008 | `["-h"]` | 0 | 0 | 0 | n/a | match | n/a | pass |
| 009 | `["-V"]` | 0 | 0 | 0 | n/a | match | n/a | pass |
| 010 | `["--autohide_time=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 011 | `["--basic=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 012 | `["--config_location"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 013 | `["--config-location"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 014 | `["--config"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 015 | `["-C"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 016 | `["--default_time_value", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 017 | `["--default_time_value", "29999ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 018 | `["--default-time-value", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 019 | `["--default-time-value", "29999ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 020 | `["-t", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 021 | `["-t", "29999ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 022 | `["--default_widget_count", "not-an-integer"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 023 | `["--default-widget-count", "not-an-integer"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 024 | `["--default_widget_type", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 025 | `["--default-widget-type", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 026 | `["--disable_click=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 027 | `["--disable_keys=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 028 | `["--dot_marker=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 029 | `["--expanded=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 030 | `["--hide_time=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 031 | `["--rate", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 032 | `["--rate", "249ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 033 | `["-r", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 034 | `["-r", "249ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 035 | `["--retention", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 036 | `["--show_table_scroll_position=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 037 | `["--time_delta", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 038 | `["--time_delta", "999ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 039 | `["--time-delta", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 040 | `["--time-delta", "999ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 041 | `["-d", "abc"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 042 | `["-d", "999ms"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 043 | `["--case_sensitive=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 044 | `["--current_usage=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 045 | `["--disable_advanced_kill=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 046 | `["--read_only=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 047 | `["--hide_k_threads=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 048 | `["--get_threads=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 049 | `["--process_default_sort", "not-a-column"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 050 | `["--process-default-sort", "not-a-column"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 051 | `["--group_processes=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 052 | `["--process_memory_as_value=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 053 | `["--process_command=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 054 | `["--regex=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 055 | `["--tree=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 056 | `["--tree_collapse=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 057 | `["--unnormalized_cpu=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 058 | `["--whole_word=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 059 | `["--celsius=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 060 | `["--fahrenheit=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 061 | `["--kelvin=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 062 | `["--cpu_left_legend=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 063 | `["--default_cpu_entry", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 064 | `["--default-cpu-entry", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 065 | `["--hide_avg_cpu=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 066 | `["--memory_legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 067 | `["--memory-legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 068 | `["--enable_cache_memory=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 069 | `["--free_arc=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 070 | `["--short_gpu_names=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 071 | `["--network_legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 072 | `["--network-legend", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 073 | `["--network_use_bytes=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 074 | `["--network_use_binary_prefix=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 075 | `["--network_use_log=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 076 | `["--use_old_network_legend=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 077 | `["--show_packets=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 078 | `["--network_start_zeroed=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 079 | `["--battery=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 080 | `["--disable_gpu=false"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 081 | `["--theme", "not-a-choice"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 082 | `["--help=invalid"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 083 | `["--version=invalid"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 084 | `["--bogus"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 085 | `["--theme", "bad"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 086 | `["--default_widget_type", "foo"]` | 2 | 2 | 2 | n/a | match | n/a | pass |
| 087 | `["--rate", "100"]` | 1 | 1 | 1 | n/a | match | n/a | pass |
| 088 | `["--rate", "250"]` | 0 | 0 | 0 | both | n/a | n/a | pass |
| 089 | `["--rate", "250ms"]` | 0 | 0 | 0 | both | n/a | n/a | pass |
| 090 | `["--retention", "59999ms"]` | 0 | 0 | 0 | both | n/a | n/a | pass |
| 091 | `["--celsius", "--kelvin"]` | 2 | 2 | 2 | n/a | match | n/a | pass |

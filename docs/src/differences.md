# Differences from btm

ntop uses [bottom (`btm`) 0.14.9](https://github.com/ClementTsang/bottom/tree/0.14.9) as its reference for screens, keys, mouse controls, and CLI flags. The D labels are ntop's project difference IDs. This page covers the defined entries D1, D2, D4-D9, and D11. D3 is a gap; no D10 behavior is defined. Each row contrasts btm's behavior with ntop's.

| ID | btm 0.14.9 | ntop |
|---|---|---|
| D1 | Passes sysinfo's process disk read and write values to its read/write rate columns on macOS. | Calculates process read and write rates from byte-counter deltas and actual elapsed sample time. |
| D2 | On Linux, divides process I/O counter deltas by whole elapsed seconds; failed division yields zero. | On Linux, divides process I/O counter deltas by fractional elapsed seconds. |
| D4 | Uses the process user ID and runtime reported by its system-information library to derive a user name and elapsed time. | Derives user names from real UIDs and elapsed time from each process's start time. |
| D5 | Uses numeric fields for process memory, CPU, and I/O; an unavailable user name displays as `N/A`. | Displays unavailable process fields as `N/A`, including memory percentage. |
| D6 | Gets disk usage from each physical partition and calculates used space as total minus available space. | On macOS, reports APFS used space per volume. |
| D7 | Uses sysinfo component labels unchanged and does not deduplicate repeated sensor names. | Shows every named temperature sensor and adds numeric suffixes such as `(1)` and `(2)` to duplicate names. |
| D8 | Discards an initial collection, waits 5 ms, then publishes the next collection. | Collects a baseline CPU sample first, then calculates the first displayed CPU usage value from the following interval. |
| D9 | Waits for the configured update interval after each collection, so collection time adds to the cycle. | Schedules repeated collection against fixed deadlines, so a long collection does not shift later deadlines. |
| D11 | The default build includes ZFS ARC collection; `--free_arc` subtracts reclaimable ARC from RAM usage. | Does not collect ZFS ARC. The `--free_arc` flag is accepted but does not change the display. |

## Unsupported behavior

ntop does not load configuration files. Passing `-C`, `--config`, `--config-location`, or `--config_location` exits with status 2.

ntop supports macOS and Linux. It exits with status 1 on other platforms, including Windows and FreeBSD.

ntop does not include Linux Intel GPU data, per-process GPU usage on macOS, or metric export.

# Differences from btm

ntop uses [bottom (`btm`) 0.14.9](https://github.com/ClementTsang/bottom/tree/0.14.9) as its reference for screens, keys, mouse controls, and CLI flags. This page covers defined entries D1, D2, D4-D9, and D11; D3 and D10 have no defined behavior.

| ID | btm 0.14.9 | ntop |
|---|---|---|
| D1 | Passes sysinfo's process disk read and write values to its read/write rate columns on macOS. | Calculates process read and write rates from byte-counter deltas and actual elapsed sample time. |
| D2 | On Linux, divides process I/O counter deltas by whole elapsed seconds; failed division yields zero. | On Linux, divides process I/O counter deltas by fractional elapsed seconds. |
| D4 | Uses the process user ID and runtime reported by its system-information library to derive a user name and elapsed time. | On macOS, reads the effective UID and start time for processes owned by other users, so their user name and elapsed time remain available when other fields are unavailable. |
| D5 | Formats process memory, CPU, and I/O from numeric values; only an unresolved user name displays as `N/A`. | Displays unavailable process values as `N/A`, including memory percentage when ntop cannot read it for another user's process on macOS. |
| D6 | Gets disk usage from each physical partition and calculates used space as total minus available space. | On macOS, reports APFS used space per volume. |
| D7 | Uses sysinfo component labels unchanged and does not deduplicate repeated sensor names. | Shows every named temperature sensor and adds numeric suffixes such as `(1)` and `(2)` to duplicate names. |
| D8 | Discards an initial collection, waits 5 ms, then publishes the next collection. | Collects a baseline CPU sample first, then calculates the first displayed CPU usage value from the following interval. |
| D9 | Waits for the configured update interval after each collection, so collection time adds to the cycle. | Schedules repeated collection against fixed deadlines, so a long collection does not shift later deadlines. |
| D11 | The default build includes ZFS ARC collection; `--free_arc` subtracts reclaimable ARC from RAM usage. | Does not collect ZFS ARC. The `--free_arc` flag is accepted but does not change the display. |

## Unsupported behavior

The project specification excludes configuration-file handling. ntop does not read or write configuration files. Passing `-C`, `--config`, `--config-location`, or `--config_location` exits with status 2.

| Argument | btm 0.14.9 | ntop |
|---|---|---|
| `-C`, `--config`, `--config-location`, `--config_location <path>` | Accepts the path and proceeds to TUI initialization. | Exits with status 2 without reading or writing the specified file. |

ntop supports macOS and Linux. It exits with status 1 on other platforms, including Windows and FreeBSD.

ntop does not include Linux Intel GPU data, per-process GPU usage on macOS, or metric export.

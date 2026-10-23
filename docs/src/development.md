# Development

## Local checks

Run `python3 scripts/check-local.py --root . --sbcl sbcl` from the repository
root. It scans `ntop.asd`, `run-tests.lisp`, and `.asd`, `.lisp`, and `.py`
files under `src`, `t`, and `scripts`, limiting each to 500 lines and 100
columns per line. Every `NTOP.*` package must have exactly `COMMON-LISP` in its
`:use` list. The check also tests that a timed-out command and its children are
terminated, and limits the SBCL package check to 60 seconds.

The macOS test, executable, and coverage commands are documented in
[Getting started](getting-started.md).

## Dependencies

`flake.nix` pins each nerima-lisp dependency to a release tag. `flake.lock`
records the resolved source revisions. Run
`python3 scripts/darwin-workflow.py dependencies` to print the dependency
report. It follows ASDF runtime dependencies and fails if the longest path
exceeds four edges between nerima-lisp repositories. The `ntop/test` depth is
reported for information and does not affect this limit. For example,
`ntop -> cl-tty-kit -> cl-concurrent-kit -> cl-boundary-kit -> cl-host-kit`
contains four edges. Test systems are reported separately.

## Continuous integration

CI runs `nix flake check --print-build-logs path:/src` in the
`nixos/nix:2.35.3` container on Linux amd64. The workflow mounts the repository
at `/src`. The job has a 40 minute limit, and the Nix container runs for at
most 30 minutes. The checks build the executable, run `ntop/test` and the same
local source checks described above, report dependency depth, and enforce 90
percent expression and branch coverage.


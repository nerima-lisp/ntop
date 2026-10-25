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

## Route harness

Run one subcase from the repository root with the project's pinned ASDF
dependencies available. Put `btm` on `PATH` and build `./ntop` first. Repeat
`--route` to select multiple routes; `--subcase` uses the full route ID and
three-digit subcase index:

```sh
sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd "ntop.asd")' \
  --eval '(asdf:load-system "ntop/e2e")' \
  --eval '
    (ntop.e2e:main
      (list "ntop-e2e" "routes" "--mode" "ntop"
            "--btm" "btm" "--ntop" "./ntop"
            "--route" "key-15" "--subcase" "key-15-002"))'
```

## Mutation testing

Run a focused measurement from the repository root. Each mutant changes one
source site in a temporary project copy, and the runner loads only the selected
test file. The runner requires SBCL and uses `nix flake archive` to resolve the
pinned dependencies. The JSON report records the baseline, mutant outcomes, and
score.

```sh
python3 scripts/mutation_test.py \
  --source src/shared/domain-rate.lisp \
  --test t/shared/domain-rate-test.lisp \
  --jobs 1 \
  --report /tmp/domain-rate-mutations.json
```

Run the empty-test calibration with the same source and test arguments plus
`--empty-test-control`. It requires zero selected tests and checks that every
generated mutant survives:

```sh
python3 scripts/mutation_test.py \
  --source src/shared/domain-rate.lisp \
  --test t/shared/domain-rate-test.lisp \
  --jobs 1 \
  --empty-test-control \
  --report /tmp/domain-rate-empty-control.json
```

Measured results for the selected source and test pairs:

| Source / selected test | Generated | Test kills | Compile rejects | Equivalent survivors | Adjusted score |
| --- | ---: | ---: | ---: | ---: | ---: |
| `src/shared/domain-rate.lisp` / `t/shared/domain-rate-test.lisp` | 8 | 8 | 0 | 0 | 100% |
| `src/shared/domain-units.lisp` / `t/shared/domain-units-test.lisp` | 49 | 43 | 2 | 4 | 100% |
| `src/feature/process/domain-grouping.lisp` / `t/feature/process/domain-grouping-test.lisp` | 13 | 9 | 3 | 1 | 100% |

The score counts test failures and production-load compile rejects as kills.
The runner reports those outcomes separately and fails a run with runner errors.
It excludes only the equivalent survivors from the adjusted score.

Equivalent survivors are `domain-grouping` M0009, which changes the initial
array capacity from 0 to 1 in `(make-array 0 :adjustable t :fill-pointer 0)` at
`src/feature/process/domain-grouping.lisp` line 60. The capacity is unobservable
through its zero fill pointer and list result. `domain-units` M0041–M0044 change
the `0` initforms at lines 30 and 36 of `src/shared/domain-units.lisp`; the
`make-quantity` and `make-scaled-quantity` constructors require explicit values,
so those initforms are not used by the supported constructors. The empty-test
control selected zero tests and left all 8 generated `domain-rate` mutants
alive.

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
most 30 minutes. The flake checks build the executable, run tests and coverage,
report dependency depth, and run `scripts/check-local.py` with the pinned SBCL
binary and a 300-second timeout.


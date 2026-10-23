# Getting started

## macOS workflow

Install Python 3, Nix, and SBCL and put them on `PATH`. The flake pins the ASDF
library sources; it does not pin these host tools. From the repository root,
run the command for the task you need:

```sh
python3 scripts/darwin-workflow.py dependencies
python3 scripts/darwin-workflow.py resolve
python3 scripts/darwin-workflow.py test
python3 scripts/darwin-workflow.py build
python3 scripts/darwin-workflow.py coverage
```

The local workflow was verified on macOS 26.6.2 with Python 3.9.6, Nix 2.34.9,
and SBCL 2.6.0. The project does not declare minimum compatible OS or host-tool
versions; other versions are unverified.

Use `resolve` alone to verify that Nix can materialize the pinned source trees
and to print their count. The other commands also resolve and archive these
sources before doing their work. An uncached GitHub input is fetched into the
Nix store. `dependencies` prints the ASDF dependency graph. `test` runs
`ntop/test`. `build` creates the `ntop` executable and prints its output path.
`coverage` writes an HTML report under `coverage/mac/` and enforces at least 90
percent expression and branch coverage.

The workflow applies a 30 minute limit to Nix and SBCL processes. It terminates
the whole child process group if a command exceeds its limit.

## Running the executable

Run the executable at the path printed by the build command. The output lives
in the local ASDF cache and is not committed.


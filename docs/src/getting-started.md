# Getting started

## Install on Linux with Nix

The flake builds for x86_64 Linux. Install [Nix](https://nix.dev/install-nix.html) if needed. Enable `nix-command` and `flakes` in Nix's active configuration. By default, Nix reads the system configuration at `/etc/nix/nix.conf` and the per-user configuration at `$HOME/.config/nix/nix.conf`; environment variables can change these locations. See the [Nix configuration reference](https://nix.dev/manual/nix/latest/command-ref/conf-file.html) for details. Add this setting to the configuration used by your installation:

```text
experimental-features = nix-command flakes
```

Check that both features are enabled:

```sh
nix show-config experimental-features
```

The output should include `nix-command` and `flakes`. Install the package with Nix:

```sh
nix profile install github:nerima-lisp/ntop
```

Then start `ntop` from a terminal:

```sh
ntop
```

## Build from source on macOS

Install Git, Python 3.9 or later, Nix, and SBCL. Use the [Python macOS installer](https://www.python.org/downloads/macos/), the [Nix installer](https://nix.dev/install-nix.html), and the [SBCL downloads page](https://sbcl.org/platform-table.html) if you need them. Enable `nix-command` and `flakes` in Nix's active configuration and check them with `nix show-config experimental-features` as described above. Check that the required programs are on `PATH` and check the Python version:

```sh
command -v git python3 nix sbcl
python3 --version
```

The first command prints a path for each program. Python must be version 3.9 or later.

Clone the repository and build the executable:

```sh
git clone https://github.com/nerima-lisp/ntop.git
cd ntop
python3 scripts/darwin-workflow.py build
```

The build command archives the pinned Lisp dependency sources with Nix and builds the executable with SBCL. Nix fetches those sources if they are not already available locally. It prints the executable path when the build completes. Run the executable at that path.

## Read the first screen

The default screen presents system metrics and a process table. The process table groups rows by process name. The sampling interval is 700 milliseconds by default.

Press `?` to open the key help. Press `q` to quit. See [Usage](usage.md) for widget controls, mouse actions, search syntax, and command-line options.

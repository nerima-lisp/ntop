# ntop

`ntop` is a Common Lisp terminal system monitor for macOS and Linux.

## Supported platforms

The Nix flake builds for x86_64 Linux. On macOS, build from source with the repository's Darwin build script.

## Install and run

On x86_64 Linux, install [Nix](https://nix.dev/install-nix.html) and enable `nix-command` and `flakes` in Nix's active configuration. See [Getting started](docs/src/getting-started.md) for the configuration locations and a check command. Install the package:

```sh
nix profile install github:nerima-lisp/ntop
ntop
```

On macOS, install Git, Python 3.9 or later, Nix, and SBCL. Enable `nix-command` and `flakes` in Nix's active configuration. From a checkout of the repository, build:

```sh
git clone https://github.com/nerima-lisp/ntop.git
cd ntop
python3 scripts/darwin-workflow.py build
```

When the pinned Lisp sources are not already available locally, Nix fetches them. Run the executable at the path printed by the build command.

Press `?` for the key help and `q` to quit. See the [documentation](docs/src/index.md) for
[getting started](docs/src/getting-started.md), [usage](docs/src/usage.md),
[btm differences](docs/src/differences.md), [architecture](docs/src/architecture.md),
and [development](docs/src/development.md).

#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

if [ "$(id -u)" -eq 0 ]; then
    printf '%s\n' 'Run this script as a normal user.' >&2
    exit 2
fi
if [ "$(uname -s)" != Darwin ]; then
    printf '%s\n' 'This comparison requires macOS.' >&2
    exit 2
fi

for executable in python3 sbcl sudo powermetrics nix; do
    if ! command -v "$executable" >/dev/null 2>&1; then
        printf 'Required command not found: %s\n' "$executable" >&2
        exit 2
    fi
done

sudo -v
exec python3 "$SCRIPT_DIR/fr002_compare.py" "$@"

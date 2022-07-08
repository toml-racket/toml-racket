#!/usr/bin/env bash
set -eu -o pipefail

rem() {
    if (($(raco pkg show "$1" | wc -l)  > 4)); then
        # Raco likes to give non-zero error codes even if removal was
        # successful, so ignore that.
        if ! raco pkg remove --batch --force "$1"; then
            printf 'First attempt to remove package "%s" failed.  Trying again...\n' "$1" >&2
            raco pkg remove --batch --force "$1"
        fi
    fi
}

rem toml
rem toml-compliance
rem toml-lib

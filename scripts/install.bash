#!/usr/bin/env bash
set -eu -o pipefail

dir="${0%/*}"
proj_dir="${dir%/*}"

cd "$proj_dir"

ins() {
    pushd "$1" >/dev/null

    set +e
    message=$(raco pkg install --auto --batch 2>&1)
    ret=$?
    set -e

    if (( ret )) && [[ $message =~ package\ is\ already\ installed ]]; then
        printf 'Package "%s" already installed, skipping.\n' "$1" >&2
    elif (( ret )); then
        printf 'Something went wrong...\n' >&2
        printf '%s\n' "$message" >&2
        exit $ret
    fi
    
    popd >/dev/null
}

ins toml-lib
ins toml-compliance
ins toml-doc
ins toml

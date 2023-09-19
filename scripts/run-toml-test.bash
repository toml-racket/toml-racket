#!/usr/bin/env bash

set -eu -o pipefail

cd "${0%/*}/../"

cmd_usage() {
    printf '%s: Run toml-tests against the racket toml library.
%s: usage: %s [-n]

  -n        : disable color output
  -l <file> : log to file
'
    case $* in
        --help|-h|help) exit 0 ;;
        *) exit 1 ;;
    esac
}

cmd_default() {
    while getopts "nl:" arg; do
        case $arg in
            n)
                export NO_COLOR='YES PLS'
                ;;
            l)
                log_to="$OPTARG"
                ;;
            *)
                cmd_usage "$@"
                ;;
        esac
    done
    shift $((OPTIND-1))

    cmd=(
        toml-test
        --
        racket -l toml/compliance/decoder
    )

    cmd_enc=(
        toml-test -encoder
        --
        racket -l toml/compliance/encoder
    )

    if [[ ${log_to+present} ]]; then
        "${cmd[@]}" 2>&1 | tee "$log_to"
        "${cmd_enc[@]}" 2>&1 | tee "$log_to"
    else
        "${cmd[@]}"
        "${cmd_enc[@]}"
    fi
}

cmd_default "$@"

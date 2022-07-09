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

    # My version of toml-test does not support 0.2.0
    cmd=(
        toml-test
        -toml 0.4.0
        --
        racket -l toml/compliance/decoder
    )

    if [[ ${log_to+present} ]]; then
        "${cmd[@]}" 2>&1 | tee "$log_to"
    else
        "${cmd[@]}"
    fi
}

cmd_default "$@"

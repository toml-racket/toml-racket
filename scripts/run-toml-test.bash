#!/usr/bin/env bash

set -eu -o pipefail

cd "${0%/*}/../"

cmd_usage() {
    printf '%s: Run toml-tests against the racket toml library.
%s: usage: %s [-n]

  -n : disable color output
'
    case $* in
        --help|-h|help) exit 0 ;;
        *) exit 1 ;;
    esac
}

cmd_default() {
    # My version of toml-test does not support 0.2.0
    toml-test -toml 0.4.0 -- racket -l toml/compliance/decoder
}

while getopts "n" arg; do
    case $arg in
        n)
            export NO_COLOR='YES PLS'
            ;;
        *)
            cmd_usage
            ;;
    esac
done

cmd_default

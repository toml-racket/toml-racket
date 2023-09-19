#!/usr/bin/env bash

set -eu -o pipefail

url='https://github.com/BurntSushi/toml-test/releases/download/v1.3.0/toml-test-v1.3.0-linux-amd64.gz'
sha256='0f5f1b92eec98c9344b97bdd8d2beabb8c64a66fa77de5b9a23a98c97e263111'
default_dest_path='/usr/local/bin/toml-test'

cmd_help() {
    printf '%s: Download and install toml-test

Usage: %s [dest-path]

  dest-path : Where to install toml-test to (%s)
' \
           "${0##*/}" \
           "${0##*/}" \
           "$default_dest_path"
    case $* in
        -h|--help|help) exit 0 ;;
        *) exit 1 ;;
    esac
}

cmd_default() {
    if [[ $# -eq 1 ]]; then
        path="$1"
    elif [[ $# -eq 0 ]]; then
        path="$default_dest_path"
    else
        cmd_help bad
    fi
    curl -L "$url" |
        gzip -dc - > "$path"

    sha256sum -c - <<EOF
$sha256 $path
EOF

    chmod 755 "$path"
}

case $* in
    --) shift; cmd_default "$@" ;;
    -*|help) cmd_help "$@" ;;
    *) cmd_default "$@" ;;
esac

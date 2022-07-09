#!/usr/bin/env bash

set -eu -o pipefail

url='https://github.com/BurntSushi/toml-test/releases/download/v1.2.0/toml-test-v1.2.0-linux-amd64.gz'
sha256='012da9aa3bcc978d6528f8de5624b51c77c4e8a63cb2a152202d62516e849756'
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

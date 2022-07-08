#!/usr/bin/env bash
set -eu -o pipefail

dir="${0%/*}"
proj_dir="${dir%/*}"

cd "$proj_dir"

ins() {
    pushd "$1" >/dev/null
    raco pkg install --auto --batch
    popd >/dev/null
}

ins toml-lib
ins toml-compliance
ins toml

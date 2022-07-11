#!/usr/bin/env bash

set -eu -o pipefail

cd "${0%/*}/.."

raco test --make -j "$(nproc)" .

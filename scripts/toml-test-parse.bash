#!/usr/bin/env bash

set -eu -o pipefail

cd "${0%/*}/.."

racket toml-test.rkt

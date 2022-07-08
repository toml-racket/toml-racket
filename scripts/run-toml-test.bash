#!/usr/bin/env bash

set -eu -o pipefail

cd "${0%/*}/../"

# My version of toml-test does not support 0.2.0
toml-test -toml 0.4.0 -- racket -l toml/compliance/decoder

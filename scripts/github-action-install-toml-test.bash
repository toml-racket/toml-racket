#!/usr/bin/env bash

set -eu -o pipefail

curl -L https://github.com/BurntSushi/toml-test/releases/download/v1.2.0/toml-test-v1.2.0-linux-amd64.gz |
    gzip -dc - > /usr/local/bin/toml-test

sha256sum -c - <<<012da9aa3bcc978d6528f8de5624b51c77c4e8a63cb2a152202d62516e849756\ /usr/local/bin/toml-test

chmod 755 /usr/local/bin/toml-test

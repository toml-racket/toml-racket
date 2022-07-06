[![Build](https://github.com/winny-/toml-racket/actions/workflows/build.yml/badge.svg)](https://github.com/winny-/toml-racket/actions/workflows/build.yml)

This is a [TOML] parser. It supports TOML **0.2.0**, including
arrays-of-tables. As of 2022-08-06 it passes 222/325 tests in [toml-test].

[TOML]: https://github.com/toml-lang/toml
[toml-test]: https://github.com/BurntSushi/toml-test

This code is a fork of [Greg Hendershott's TOML parser](https://github.com/greghendershott/toml).  This version is available on pkgs.racket-lang.org.

## Installation

```bash
raco pkg install --auto toml
```

## Usage

```racket
(require toml)
(parse-toml s) ;; where `s` is a `string?`
```

## Goals

- Pass all [toml-test] tests for TOML **0.2.0**.

- Provide useful error messages with positions (line:col:ofs). Do so
  for both syntax errors and semantic errors (such as table
  conflicts).

- Return a Racket `hasheq` that satisfies the `json` library's
  `jsexpr?` predicate, so that you can run it through `jsexpr->string`
  to produce a JSON string.

  > Caveat: Any TOML datetime values are `date` structs, which won't
  satisfy `jsexpr?`. Originally I parsed these to a
  `current-seconds`-style integer value. But `toml-tests` needs things
  to be tagged with types, so that's why I had to switch. I should
  probably provide a conversion function to turn any such instances
  back into a datetime string so that it can be passed to
  `jsexpr->string`.

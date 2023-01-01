[![Tests](https://github.com/winny-/toml-racket/actions/workflows/tests.yml/badge.svg)](https://github.com/winny-/toml-racket/actions/workflows/tests.yml) [![TOML Compliance](https://github.com/winny-/toml-racket/actions/workflows/compliance.yml/badge.svg)](https://github.com/winny-/toml-racket/actions/workflows/compliance.yml) [![raco pkg install toml](https://img.shields.io/badge/raco%20pkg%20install-toml-purple)](https://pkgs.racket-lang.org/package/toml)


This is a [TOML] parser, with dates supporting nanosecond precision. It
passes all tests in [toml-test] v1.3.0.
Click the "TOML Compliance" badge for logs of toml-test.

[TOML]: https://github.com/toml-lang/toml
[toml-test]: https://github.com/BurntSushi/toml-test

This code is a fork of [Greg Hendershott's TOML parser](https://github.com/greghendershott/toml).
Unlike that repository, this version is available on pkgs.racket-lang.org.

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

- Pass all [toml-test] tests for TOML **1.0.0**.

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

#lang scribble/manual

@(require (for-label (except-in racket/base date)
                     (prefix-in base: (only-in racket/base date))
                     json))

@(define website @link["https://toml.io"]{TOML web site})
@(define repo @link["https://github.com/toml-racket/toml-racket"]{GitHub repository})
@(begin (require scribble/eval)
        (define ev (make-base-eval))
        (ev '(require toml)))

@title{TOML}

@author{Greg Hendershott, Winston Weinert, and Benjamin Yeung}

@defmodule[toml]

This library provides utilities for parsing the TOML configuration file format to Racket values.
See the @website for more information about TOML.

Active development is ongoing. Please see the @repo for more information.

@; -----------------------------------------------------------------------------
@section{Parsing TOML Text}

@defproc[(parse-toml [str string?])
         hasheq?]{
  Parses the input string, returning a @racket[hasheq] for the hash table
  represented by the string if it matches the TOML standard. Raises an error
  if the string is not valid TOML.

  Dates are returned using @racket[gregor] structs.

@examples[#:eval ev
  (parse-toml
   (string-append
    "[a.b]\n"
    "c = 1\n"
    "[a]\n"
    "d = 2"))
  (parse-toml "today = 2014-06-26T12:34:56Z")
  (parse-toml
   (string-append
    "[a]\n"
    "b = 1\n"
    "[a]\n"
    "c = 2"))
  (parse-toml
   (string-append
    "x = 1\n"
    "x = 2\n"))
  (parse-toml
   (string-append
    "[fruit]\n"
    "apple.color = \"red\"\n"
    "apple.taste.sweet = true\n"
    "\n"
    "[fruit.apple]"))]
}

@section{Discussion of Design and Future Plans}

Here are a few caveats or "gotchas" that may be relevant to users.
Please visit the @repo and comment if these are issues of concern.

@subsection{Use of @racket[gregor]}

The TOML specification requires that dates and times can be represented
separately or in combination, with millisecond (or greater) precision.
These factors led to requiring the @racket[gregor] library and using
those structs instead of the base Racket @racketlink[base:date]{date}
and related apparatus with custom machinery.

This means that the resulting output is not guaranteed to satsify
@racket[jsexpr?]. Future plans are to eventually expand the library API
to include a function that parses TOML to ensure @racket[jsexpr] output.

Another consequence is that even if an application does not need all
of the date and time features that the TOML spec allows, @racket[gregor]
is currently still a dependency.

@subsection{Error messages for table conflicts}

The current parser design allows for the error messages to include
position information when a key has been defined multiple times.
However, for tables, it only provides the table name (and not the
in-file position) of the offending situation.

@subsection{Automatic generation of output TOML string}

There is currently not an automated generator of TOML output; this
library only provides the single parser for reading TOML strings.

@close-eval[ev]

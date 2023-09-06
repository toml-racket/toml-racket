#lang scribble/manual

@(require (for-label (except-in racket/base date date? time)
                     (prefix-in base: (only-in racket/base date))
                     gregor
                     gregor/time
                     json))

@(define website @link["https://toml.io"]{TOML web site})
@(define repo @link["https://github.com/toml-racket/toml-racket"]{GitHub repository})
@(define gregor @seclink["top" #:doc '(lib "gregor/scribblings/gregor.scrbl")]{gregor})
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
@section{TOML-expressions}

A @deftech{tomlexpr} is a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{hash table}
with @racket[symbol?] keys and values being one of:

@itemize[
 @item{@racket[boolean?]}
 @item{@racket[number?]}
 @item{@racket[string?]}
 @item{@racket[moment?]}
 @item{@racket[datetime?]}
 @item{@racket[date?]}
 @item{@racket[time?]}
 @item{a @tech{tomlexpr}}
 @item{@racket[list?], with elements consisting of a mixture of any of the above types}]

@defproc[(tomlexpr? [x any/c])
         boolean?]{
  Performs a deep check to determine whether @racket[x] is a @tech{tomlexpr}.

@examples[#:eval ev
  (tomlexpr? '#hasheq((n . 0)))
  (tomlexpr? '#hash((x . +inf.0)))
  (tomlexpr? 5)
  (tomlexpr? '#hash(("oops" . "keys must be symbols")))

  (require gregor/time)
  (tomlexpr? `#hasheq((xs . (1 "b" #hash((noon . ,(time 12)))))))]
}

@; -----------------------------------------------------------------------------
@section{Parsing TOML Text}

@defproc[(parse-toml [str string?])
         hasheq?]{
  Parses the input string, returning a @racket[hasheq] for the hash table
  represented by the string if it matches the TOML standard. Raises an error
  if the string is not valid TOML.

  Dates are returned using @gregor structs.

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

@; -----------------------------------------------------------------------------
@section{Generating TOML Text from TOML-expressions}

@defproc[(tomlexpr->string [x tomlexpr?])
         string?]{
  Returns a TOML string representing the @tech{tomlexpr} @racket[x].

  This function should be considered unstable. The output of
  @racket[(tomlexpr->string x)] will continue to be a valid TOML string, but
  the exact formatting properties of the string should not be relied upon to
  remain fixed. Future versions may also add more arguments to control various
  aspects of this function.
  
  The current properties of the string include:

  @itemize[
    @item{Newlines are always LF, never CRLF.}
    @item{Keys will be ordered according to @racket[string<?] applied to the
     @racket[symbol->string] of the keys.}
    @item{Dotted keys are never used for terminal key/value pairs, though they
     may be used for table headers.}
    @item{Inline table syntax is used only when necessary in lists.}
    @item{Lists will be spread across multiple lines with a trailing comma for
     the final element.}]

@examples[#:eval ev
  (tomlexpr->string '#hasheq((|another-table| . #hasheq((key . 10)))
                             (table . #hasheq((key . 5)
                             (array . (#hasheq((a . 1) (b . 2))
                                       #hasheq((a . 2) (b . 4))))))))
  (tomlexpr->string '#hasheq((list . (1 2 3))))
  (tomlexpr->string '#hasheq((contributors
                              .
                              ("Foo Bar <foo@example.com>"
                               #hasheq((name . "Baz Qux")
                                       (email . "bazqux@example.com")
                                       (url . "https://example.com/bazqux"))))))]
}

@; -----------------------------------------------------------------------------
@section{Discussion of Design and Future Plans}

Here are a few caveats or "gotchas" that may be relevant to users.
Please visit the @repo and comment if these are issues of concern.

@subsection{TOML and JSON}

While TOML and JSON have slightly different intended purposes, both can
naturally represent symbol-keyed hash tables. There are some slight differences
between the two, with neither being a strict subset of the other. TOML maps
specifically to a hash table, where JSON need not be. However, TOML's
specification includes dates as basic types and mandates support for infinity
and NaN.

As a consequence, a @tech{tomlexpr} is not guaranteed to satisify
@racket[jsexpr?]. Future plans are to eventually expand the library API
to include a mechanism to provide @racket[jsexpr] output, though individual
users may want to consider how/if their application should handle non-JSON
values.

@subsection{Use of @gregor}

The TOML specification requires that dates and times can be represented
separately or in combination, with millisecond (or greater) precision.
These factors led to requiring the @gregor library and using
those structs instead of the base Racket @racketlink[base:date]{date}
and related apparatus with custom machinery.

As generating these dates are part of @racket[parse-toml], this means that
this library in its current state still adds @gregor as a dependency,
even if an application does not need all of the date and time features that
the TOML spec allows.

@subsection{Formatting of generated TOML strings}

TOML permits many different ways to represent the same hash table.
The formatting used for this version prioritizes simple testing and
implementation. More options may eventually be exposed, but at this (early)
stage, the default could change quite substantially.

@subsection{Error messages for table conflicts}

The current parser design allows for the error messages to include
position information when a key has been defined multiple times.
However, for tables, it only provides the table name (and not the
in-file position) of the offending situation.

@subsection{Automatic generation of output TOML string}

There is currently not an automated generator of TOML output; this
library only provides the single parser for reading TOML strings.

@close-eval[ev]

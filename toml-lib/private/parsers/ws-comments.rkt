#lang racket/base

(require racket/list
         racket/function
         racket/match
         racket/math

         "../parsack.rkt"
         "../misc.rkt"
         "../stx.rkt"

         "./shared.rkt")

(provide (all-defined-out))

(define $space-char
  (<?> (oneOf " \t") "space or tab"))

(define $sp
  (<?> (many $space-char)
       "zero or more spaces or tabs"))

(define $nl
  (<?> (<or> (string "\r\n")
             (char #\newline))
       "Newline (LF or CRLF"))

(define $spnl
  (<?> (pdo $sp (optional $nl) $sp
            (return null))
       "zero or more spaces, and optional newline plus zero or more spaces"))

(define $blank-line
  (<?> (try (pdo $sp $nl (return (void))))
       "blank line"))

(define $comment-char
  (<?> (<or> (char #\tab)
             (satisfy (λ (c)
                        ;; Between space ( ) and tilde (~).
                        (<= #x20 (char->integer c) #x7e)))
             $non-ascii)
       "comment character"))

(define $comment
  (<?> (try (pdo $sp (char #\#) (manyUntil $comment-char $nl)
                 (return null)))
       "comment"))

(define $blank-or-comment-line
  (<or> $blank-line $comment))

#lang racket/base

(require racket/list
         racket/function
         racket/match
         racket/math

         "../parsack.rkt"
         "../misc.rkt"
         "../stx.rkt")

(provide (all-defined-out))

(define $space-char
  (<?> (oneOf " \t") "space or tab"))

(define $sp
  (<?> (many $space-char)
       "zero or more spaces or tabs"))

(define $spnl
  (<?> (pdo $sp (optional (char #\return)) (optional $newline) $sp
            (return null))
       "zero or more spaces, and optional newline plus zero or more spaces"))

(define $blank-line
  (<?> (try (pdo $sp $newline (return (void))))
       "blank line"))

(define $comment
  (<?> (try (pdo $sp (char #\#) (manyUntil $anyChar $newline)
                 (return null)))
       "comment"))

(define $blank-or-comment-line
  (<or> $blank-line $comment))

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
             $newline)
       "newline (LF or CRLF)"))

(define $comment-char
  (<?> (<or> (char #\tab)
             (satisfy (λ (c)
                        ;; Between space ( ) and tilde (~).
                        (<= #x20 (char->integer c) #x7e)))
             $non-ascii)
       "comment character"))

(define $comment
  (<?> (try (pdo (char #\#) (manyUntil $comment-char (lookAhead (<or> $eof $nl)))
                 (return null)))
       "comment"))

(define $sp-maybe-comment
  (pdo $sp (optional $comment)))

(define $ws-or-comments
  (<?> (pdo (many (try (pdo $sp-maybe-comment $nl))) $sp)
       "whitespace (possibly with comments)"))

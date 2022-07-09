#lang racket/base

(require racket/list
         racket/function
         racket/match
         racket/math

         "../parsack.rkt"
         "../misc.rkt"
         "../stx.rkt"

         "ws-comments.rkt")

(provide (all-defined-out))


(define $string-char
  (<?> (<or> (pdo
              (char #\\)
              (<or> (>> (char #\b) (return #\backspace))
                    (>> (char #\n) (return #\newline))
                    (>> (char #\f) (return #\page))
                    (>> (char #\r) (return #\return))
                    (>> (char #\t) (return #\tab))
                    (>> (char #\\) (return #\\))
                    (>> (char #\") (return #\"))
                    (>> (char #\/) (return #\/))
                    (pdo (oneOf "uU")
                         (cs <- (many $hexDigit))
                         (return
                          (integer->char (string->number (list->string cs)
                                                         16))))
                    ))
             (noneOf "\"\\"))
       "character or escape character"))

(define $basic-string
  (<?> (try (pdo (char #\")
                 (cs <- (manyUntil $string-char (char #\")))
                 (return (list->string cs))))
       "multi-line basicstring"))

(define $ml-basic-string
  (<?> (try (pdo (string "\"\"\"")
                 (cs <- (manyUntil $string-char (string "\"\"\"")))
                 (return (list->string cs))))
       "multi-line basic string"))

(define $lit-string
  (<?> (try (pdo (char #\')
                 (cs <- (manyUntil $anyChar (char #\')))
                 (return (list->string cs))))
       "literal string"))

(define $ml-lit-string
  (<?> (try (pdo (string "'''")
                 (cs <- (manyUntil $anyChar (string "'''")))
                 (return (list->string cs))))
       "multi-line literal string"))

(define $string
  (<?> (try (<or> $basic-string
                  $ml-basic-string
                  $lit-string
                  $ml-lit-string))
       "string"))

(define $optional-sign
  (<or> (>> (char #\-) (return '(#\-)))
        (>> (char #\+) (return '(#\+)))
        (return '())))

(define (make-$underscore-separated $parser)
  (define $inner
    (pdo (v <- $parser)
         (w <- (<or>
                (pdo (optional (char #\_))
                     (i <- $inner)
                     (return i))
                (return null)))
         (return (cons v w))))
  (pdo
   (v <- $inner)
   (return (list->string v))))

(define $dec-int
  (<?> (try (pdo
             (sign <- $optional-sign)
             (<or> (>> (char #\0) (return 0))
                   (pdo (s <- (make-$underscore-separated $digit))
                        (return (let ([n (string->number s)])
                                  (match sign
                                    [(list #\-) (- n)]
                                    [_ n])))))))
       "decimal integer"))

(define $hex-int
  (<?> (try (pdo
             (string "0x")
             (s <- (make-$underscore-separated $hexDigit))
             (return (string->number s 16))))
       "hex integer"))

(define $bin-int
  (<?> (try (pdo
             (string "0b")
             (s <- (make-$underscore-separated (oneOf "01")))
             (return (string->number s 2))))
       "binary integer"))

(define $oct-int
  (<?> (try (pdo
             (string "0o")
             (s <- (make-$underscore-separated (oneOf "01234567")))
             (return (string->number s 8))))
       "binary integer"))


(define $integer
  (<?> (try (<or> $hex-int
                  $bin-int
                  $oct-int
                  $dec-int))
       "integer"))

(define $special-float
  (<?> (try (pdo (sign <- $optional-sign)
                 (<or> (>> (string "nan")
                           (return (match sign
                                     [(list #\-) -nan.0]
                                     [_  +nan.0])))
                       (>> (string "inf")
                           (return (match sign
                                     [(list #\-) -inf.0]
                                     [_ +inf.0]))))))
       "Special float (nan/inf)"))

(define $zero-prefixable-int
  (<?> (try (pdo (s <- (make-$underscore-separated $digit))
                 (return (string->number s))))
       "Zero prefixable integer"))

(define $float
  (<?>
   (try
    (<or> $special-float
          (pdo (i <- $dec-int)
               (<or> (try (pdo (exp <- $exp)
                               (return (string->number (format "~ae~a" i exp)))))
                     (try (pdo (char #\.)
                               (f <- $zero-prefixable-int)
                               (exp <- (<or> $exp (return null)))
                               (return (string->number
                                        (format "~a.~a~a"
                                                i
                                                f (if (null? exp)
                                                      ""
                                                      (format "e~a" exp)))))))))))
   "float"))

(define $exp
  (<?> (try (pdo
             (char #\e)
             (sign <- $optional-sign)
             (i <- $zero-prefixable-int)
             (return (match sign
                       [(list #\-) (- i)]
                       [_ i]))))
       "Float exponent"))

(define $true  (pdo (string "true")  (return #t)))
(define $false (pdo (string "false") (return #f)))

(define ->num (compose string->number list->string list))
(define $4d (pdo-seq $digit $digit $digit $digit #:combine-with ->num))
(define $2d (pdo-seq $digit $digit #:combine-with ->num))

(define $datetime
  ;; 1979-05-27T07:32:00Z
  (try (pdo (yr <- $4d) (char #\-) (mo <- $2d) (char #\-) (dy <- $2d)
            (char #\T)
            (hr <- $2d) (char #\:) (mn <- $2d) (char #\:) (sc <- $2d)
            (char #\Z)
            (return (date sc mn hr dy mo yr 0 0 #f 0)))))

(define $array
  (<?>
   (<or>
    (try
     (pdo $sp
          (char #\[)
          $spnl (many $blank-or-comment-line) $sp
          (v <- $val)
          (vs <- (many (try (pdo
                             $spnl
                             (char #\,)
                             $spnl
                             (many $blank-or-comment-line)
                             $sp
                             (vn <- $val)
                             (return vn)))))
          $spnl
          (optional (char #\,))
          $spnl
          (many $blank-or-comment-line)
          $spnl
          (char #\])
          (return (cons v vs))))
    (try
     (pdo $sp
          (char #\[)
          $spnl (many $blank-or-comment-line)
          (char #\])
          (return null))))
   "Array"))

(define $val
  (<or> $true
        $false ;before $numeric. "fa" in "false" could be hex
        $datetime ;before $numeric. dates start with number
        $float
        $integer
        $string
        $array))

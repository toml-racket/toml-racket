#lang racket/base

(require gregor
         gregor/time
         racket/list
         racket/function
         racket/match
         racket/math

         "../parsack.rkt"
         "../misc.rkt"
         "../stx.rkt"

         "ws-comments.rkt"
         "shared.rkt")

(provide (all-defined-out))

(define $escape
  (<?>
   (pdo (char #\\)
        (<or> (>> (char #\") (return #\"))
              (>> (char #\\) (return #\\))
              (>> (char #\b) (return #\backspace))
              (>> (char #\f) (return #\page))
              (>> (char #\n) (return #\newline))
              (>> (char #\r) (return #\return))
              (>> (char #\t) (return #\tab))
              (pdo (oneOf "uU")
                   (cs <- (<or> (try (repetition $hexDigit 8))
                                (repetition $hexDigit 4)))
                   (return
                    (integer->char (string->number (list->string cs)
                                                   16))))))
   "String escape sequence"))

;; Per TOML v1.0.0, any Unicode character except those that must be escaped: quotation mark,
;; backslash, and the control characters other than tab (U+0000 to U+0008, U+000A to U+001F, U+007F).
(define $basic-char
  (<?> (<or> $escape
             $space-char
             (char #\!)
             (char-range #\# #\[)
             (char-range #\] #\~)
             $non-ascii)
       "character or escape character"))

(define $ml-content
  (<?> (<or> (try (pdo (char #\\) $sp $nl
                       (skipMany (<or> $space-char $nl))))
             $nl
             (char #\")
             $basic-char)
       "multiline basic string content"))

;; Per TOML v1.0.0, no escapes, no single quotes, no control character other than tab.
(define $lit-string-char
  (<?> (<or> (char #\tab)
             (char-range #\space #\&)
             (char-range #\( #\~)
             $non-ascii)
       "literal string char"))

(define $ll-content
  (<?> (<or> $nl
             (char #\')
             $lit-string-char)
       "multiline literal string content"))

;; Returns list of 0-2 of the results of a given parser.
;; Intended for use to check for additional multi-line string quotes.
(define (up-to-2 p)
  (pdo (a <- (option #f p))
       (b <- (option #f p))
       (return (match* (a b)
                 [(#f #f) '()]
                 [(a #f) (list a)]
                 [(a b) (list a b)]))))

(define $basic-string
  (<?> (try (pdo (char #\")
                 (cs <- (manyUntil $basic-char (char #\")))
                 (return (list->string cs))))
       "basic string"))

(define $ml-basic-string
  (<?> (try (pdo (string "\"\"\"")
                 (optional $nl)
                 (cs <- (manyUntil $ml-content (try (string "\"\"\""))))
                 (extras <- (up-to-2 (char #\")))
                 ; $ml-content returns null for line-ending backslash
                 (return (list->string (append (filter char? cs) extras)))))
       "multi-line basic string"))

(define $lit-string
  (<?> (try (pdo (char #\')
                 (cs <- (manyUntil $lit-string-char (char #\')))
                 (return (list->string cs))))
       "literal string"))

(define $ml-lit-string
  (<?> (try (pdo (string "'''")
                 (optional $nl)
                 (cs <- (manyUntil $ll-content (try (string "'''"))))
                 (extras <- (up-to-2 (char #\')))
                 (return (list->string (append cs extras)))))
       "multi-line literal string"))

(define $string
  (<?> (try (<or> $ml-basic-string
                  $basic-string
                  $ml-lit-string
                  $lit-string))
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
             (oneOf "eE")
             (sign <- $optional-sign)
             (i <- $zero-prefixable-int)
             (return (match sign
                       [(list #\-) (- i)]
                       [_ i]))))
       "Float exponent"))

(define $true  (pdo (string "true")  (return #t)))
(define $false (pdo (string "false") (return #f)))

;;--------------------------------------------------
;; Date parsers
;;--------------------------------------------------

(define ->num (compose string->number list->string list))
(define $4d (pdo-seq $digit $digit $digit $digit #:combine-with ->num))
(define $2d (pdo-seq $digit $digit #:combine-with ->num))

(define (->ns ds)
  (define padding (- 9 (length ds)))
  (string->number
   (list->string
    (if (positive? padding)
        (append ds (make-list padding #\0))
        (take ds 9)))))
(define $ns (pdo-seq (many $digit) #:combine-with ->ns))
(define $s/ns
  (pdo (s <- $2d)
       (option (cons s 0)
               (pdo (char #\.)
                    (ns <- $ns)
                    (return (cons s ns))))))

(define $ymd
  (pdo (yr <- $4d) (char #\-) (mo <- $2d) (char #\-) (dy <- $2d)
       (return (list yr mo dy))))

(define $hms
  (pdo (h <- $2d) (char #\:) (m <- $2d) (char #\:) (s/ns <- $s/ns)
       (return (list h m (car s/ns) (cdr s/ns)))))

(define $offset
  (<or> (pdo (oneOf "Zz")
             (return 0))
        (pdo (sign <- (oneOf "+-")) (h <- $2d) (char #\:) (m <- $2d)
             (return (let [(tz (+ (* 3600 h) (* 60 m)))]
                       (match sign
                         [#\- (- tz)]
                         [_ tz]))))))

;; Parser for all date/time types specified by TOML v1.0.0 and toml-test.
(define $datetime
  (<?> (<or> (try (pdo (ymd <- $ymd)
                       (option (apply date ymd)
                               (try (pdo (oneOf "Tt ")
                                         (hms <- $hms)
                                         (option (apply datetime (append ymd hms))
                                                 (try (pdo (tz <- $offset)
                                                           (return (apply moment
                                                                          (append ymd hms)
                                                                          #:tz tz))))))))))
             (try (pdo (hms <- $hms)
                       (return (apply time hms)))))
       "datetime (offset or local)"))

(define $array
  (<?>
   (<or>
    (try
     (pdo $sp
          (char #\[)
          $ws-or-comments
          (v <- $val)
          (vs <- (many (try (pdo
                             $ws-or-comments
                             (char #\,)
                             $ws-or-comments
                             (vn <- $val)
                             (return vn)))))
          $ws-or-comments
          (optional (char #\,))
          $ws-or-comments
          (char #\])
          (return (cons v vs))))
    (try
     (pdo $sp
          (char #\[)
          $ws-or-comments
          (char #\])
          (return null))))
   "Array"))

;;--------------------------------------------------
;; Keys for key = val pairs and for tables and arrays of tables
;;--------------------------------------------------

(define $key-component
  (pdo $sp
       (v <-
          (<or> (pdo (s <- (many1 (<or>
                                   (char-range #\a #\z)
                                   (char-range #\A #\Z)
                                   $digit
                                   (oneOf "_-"))))
                     (return (list->string s)))
                $lit-string
                $basic-string))
       $sp
       (return (string->symbol v))))

;; Valid chars for both normal keys and table keys
(define (make-$key blame)
  (<?>
   (pdo (cs <- (sepBy1 $key-component (char #\.)))
        (return cs))
   blame))

(define $key/val ;; >> (list/c symbol? stx?)
  (try (pdo (key <- (make-$key "key"))
            $sp
            (char #\=)
            $sp
            (pos <- (getPosition))
            (val <- $val)
            (return (list key (stx val pos))))))

(define $inline-table
  (<?> (try (pdo (char #\{)
                 $sp
                 (kvs <- (sepBy $key/val (try (pdo $sp (char #\,) $sp))))
                 $sp
                 (char #\})
                 (return (stx->dat (kvs->hasheq '() kvs)))))
       "inline table"))

(define $val
  (<or> $true
        $false ;before $numeric. "fa" in "false" could be hex
        $datetime ;before $numeric. dates start with number
        $float
        $integer
        $string
        $array
        $inline-table))

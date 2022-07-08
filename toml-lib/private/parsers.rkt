#lang racket/base

(require racket/list
         racket/function
         racket/match
         racket/math

         "parsack.rkt"
         "misc.rkt"
         "stx.rkt")

(provide (all-defined-out))

;;; Whitespace and comments

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

;;; Literal values

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

(define ($array state) ($_array state)) ;; "forward decl"

(define $val
  (<or> $true
        $false ;before $numeric. "fa" in "false" could be hex
        $datetime ;before $numeric. dates start with number
        $float
        $integer
        $string
        $array))

;; TOML arrays require items to have same type. To handle this with
;; parsing (vs. semantically), we insist that same literal parser be
;; used for all items.
(define (array-of $value-parser)
  (try (pdo $sp
            (char #\[)
            $spnl (many $blank-or-comment-line) $sp
            (v <- $value-parser)
            (vs <- (many (try (pdo
                               $spnl
                               (char #\,)
                               $spnl
                               (many $blank-or-comment-line)
                               $sp
                               (vn <- $value-parser)
                               (return vn)))))
            $spnl
            (optional (char #\,))
            $spnl
            (many $blank-or-comment-line)
            $spnl
            (char #\])
            (return (cons v vs)))))

(define $empty-array
  (try (pdo $sp
            (char #\[)
            $spnl (many $blank-or-comment-line)
            (char #\])
            (return null))))

(define $_array
  (<or>
   $empty-array
   (array-of (<or> $true $false))
   (array-of $datetime)
   (array-of $float)
   (array-of $integer)
   (array-of $string)
   (array-of $array)))

;;; Keys for key = val pairs and for tables and arrays of tables

(define $key-component
  (pdo $sp
       (v <-
          (<or> (pdo (s <- (many1 (<or> $alphaNum (oneOf "_-"))))
                     (return (list->string s)))
                $string))
       $sp
       (return (string->symbol v))))

;; Valid chars for both normal keys and table keys
(define (make-$key blame)
  (<?>
   (pdo (cs <- (sepBy1 $key-component (char #\.)))
        (return cs))
   blame))

(define $common-key-char
  (<or> $alphaNum (oneOf "_-")))

(define $table-key-char
  (<or> $common-key-char (oneOf " ")))

(define $key-char
  (<or> $common-key-char (oneOf "[].")))

(define $table-key ;; >> symbol?
  (<?> (pdo (cs <- (many1 $table-key-char))
            (return (string->symbol (list->string cs))))
       "table key"))

(define $key ;; >> symbol?
  (<?> (pdo (cs <- (many1 $key-char))
            (return (string->symbol (list->string cs))))
       "key"))

(define $key/val ;; >> (list/c symbol? stx?)
  (try (pdo $sp
            (key <- (make-$key "key"))
            $sp
            (char #\=)
            $sp
            (pos <- (getPosition))
            (val <- $val)
            $sp
            (<or> $comment $newline)
            (many $blank-or-comment-line)
            $sp
            (return (list key (stx val pos))))))

;;; Table keys, handled as #\. separated

(define $table-keys ;; >> (listof symbol?)
  (make-$key "table key"))

(define (table-keys-under parent-keys)
  (pdo (if (empty? parent-keys)
           (return null)
           (pdo (string (keys->string parent-keys))
                (char #\.)))
       (keys <- $table-keys)
       (return (append parent-keys keys))))

;;; Tables

(define (table-under parent-keys)
  (<?> (try (pdo $sp
                 (keys <- (between (char #\[) (char #\])
                                   (table-keys-under parent-keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (many $blank-or-comment-line)
                 $sp
                 (return (kvs->hasheq keys kvs))))
       "table"))

(define $table (table-under '()))

;;; Arrays of tables

(define (array-of-tables-under parent-keys)
  (<?> (try (pdo $sp
                 (keys <- (between (string "[[") (string "]]")
                                   (table-keys-under parent-keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (tbs  <- (many (<or> (table-under keys)
                                      (array-of-tables-under keys))))
                 (aots <- (many (array-of-tables-same keys)))
                 (many $blank-or-comment-line)
                 $sp
                 (return
                  (let* ([tbs (map (curryr hash-refs keys) tbs)] ;hoist up
                         [aot0 (merge (cons (kvs->hasheq '() kvs) tbs)
                                      keys)]
                         [aots (cons aot0 aots)])
                    (match-define (list all-but-k ... k) keys)
                    (kvs->hasheq all-but-k
                                 (list (list k aots)))))))
       "array-of-tables"))

(define (array-of-tables-same keys)
  (<?> (try (pdo $sp
                 (between (string "[[") (string "]]")
                          (string (keys->string keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (tbs  <- (many (<or> (table-under keys)
                                      (array-of-tables-under keys))))
                 (many $blank-or-comment-line)
                 $sp
                 (return
                  (let ([tbs (map (curryr hash-refs keys) tbs)]) ;hoist up
                    (merge (cons (kvs->hasheq '() kvs) tbs)
                           keys)))))
       "array-of-tables"))

(define $array-of-tables (array-of-tables-under '()))

;;; A complete TOML document

(define $toml-document
  (pdo (many $blank-or-comment-line)
       (kvs <- (many $key/val))
       (tbs <- (many (<or> $table $array-of-tables)))
       (many $blank-or-comment-line)
       $eof
       (return (merge (cons (kvs->hasheq '() kvs) tbs)
                      '()))))

;;; Main, public function. Returns a `hasheq` using the same
;;; conventions as the Racket `json` library. e.g. You should be able
;;; to give the result to `jsexpr->string`. EXCEPTION: TOML datetimes
;;; are parsed to Racket `date` struct values, which do NOT satisfy
;;; `jsexpr?`.
(define (parse-toml s) ;; string? -> almost-jsexpr?
  (stx->dat (parse-result $toml-document (string-append s "\n\n\n"))))

#lang racket/base

(require racket/list
         racket/function
         racket/match
         racket/math
         racket/port

         "../parsack.rkt"
         "../misc.rkt"
         "../stx.rkt"

         "literals.rkt"
         "ws-comments.rkt"
         "shared.rkt")

(provide exn:fail:parsack?
         (all-from-out "literals.rkt")
         (all-from-out "ws-comments.rkt")
         (all-from-out "shared.rkt")
         (all-defined-out))

;;; Keys for key = val pairs and for tables and arrays of tables

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
(define (parse-toml input) ;; string? -> almost-jsexpr?
  (define toml-bytes
    (bytes-append
     (cond [(input-port? input) (port->bytes input)]
           [(bytes? input) input]
           [(string? input) (string->bytes/utf-8 input)]
           [else (raise-argument-error 'parse-toml "input is not an input-port?, bytes?, or string?." input)])
     ;; XXX Parser should not require these newlines.
     #"\n\n\n"))
  (stx->dat (parse-result $toml-document
                          (open-input-bytes toml-bytes))))

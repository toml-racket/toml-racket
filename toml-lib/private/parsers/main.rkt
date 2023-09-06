#lang racket/base

(require racket/list
         racket/function
         racket/match
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

;; Newline-delimited key/value pairs (as opposed to inline table-style)
(define $kv-lines
  (many (pdo (kv <- $key/val)
             $sp-maybe-comment
             ;; HACK for last-line key/value pairs
             (<or> $eof
                   (pdo $nl $ws-or-comments))
             (return kv))))

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
  (<?> (try (pdo (keys <- (between (char #\[) (char #\])
                                   (table-keys-under parent-keys)))
                 $sp-maybe-comment
                 ;; HACK to allow last-line headers
                 (<or> (pdo $eof
                            (return (kvs->table keys '())))
                       (pdo $nl
                            $ws-or-comments
                            (kvs <- $kv-lines)
                            $ws-or-comments
                            (return (kvs->table keys kvs))))))
       "table"))

(define $table (table-under '()))

;;; Arrays of tables

(define (array-of-tables-under parent-keys)
  (<?> (try (pdo (keys <- (between (string "[[") (string "]]")
                                   (table-keys-under parent-keys)))
                 $sp-maybe-comment
                 ;; HACK to allow last-line headers
                 (<or> (pdo $eof
                            (return
                             (match-let ([(list all-but-k ... k) keys])
                               (kvs->table all-but-k
                                           (list (list (list k) (hasheq)))
                                           #t))))
                       (pdo $nl
                            $ws-or-comments
                            (kvs <- $kv-lines)
                            (tbs  <- (many (<or> (table-under keys)
                                                 (array-of-tables-under keys))))
                            (aots <- (many (array-of-tables-same keys)))
                            $ws-or-comments
                            (return
                             (let* ([tbs (map (curryr change-root keys) tbs)] ;hoist up
                                    [aot0 (merge (cons (kvs->table '() kvs) tbs)
                                                 keys)]
                                    [aots (cons aot0 aots)])
                               (match-define (list all-but-k ... k) keys)
                               (kvs->table all-but-k
                                           (list (list (list k) aots))
                                           #t)))))))
       "array-of-tables"))

(define (array-of-tables-same keys)
  (<?> (try (pdo (between (string "[[") (string "]]")
                          (string (keys->string keys)))
                 $sp-maybe-comment $nl
                 $ws-or-comments
                 (kvs <- $kv-lines)
                 (tbs  <- (many (<or> (table-under keys)
                                      (array-of-tables-under keys))))
                 $ws-or-comments
                 (return
                  (let ([tbs (map (curryr change-root keys) tbs)]) ;hoist up
                    (merge (cons (kvs->table '() kvs) tbs)
                           keys)))))
       "array-of-tables"))

(define $array-of-tables (array-of-tables-under '()))

;;; A complete TOML document

(define $toml-document
  (pdo $ws-or-comments
       (kvs <- $kv-lines)
       (tbs <- (many (<or> $table $array-of-tables)))
       $ws-or-comments
       (optional $comment)
       $eof
       (return (merge (cons (kvs->table '() kvs) tbs)
                      '()))))

;;; Main, public function. Returns a `hasheq` using the same
;;; conventions as the Racket `json` library. e.g. You should be able
;;; to give the result to `jsexpr->string`. EXCEPTION: TOML datetimes
;;; are parsed to Racket (gregor library) `date` struct values,
;;; which do NOT satisfy `jsexpr?`.
(define (parse-toml input) ;; string? -> almost-jsexpr?
  (define toml-bytes
    (bytes-append
     (cond [(input-port? input) (port->bytes input)]
           [(bytes? input) input]
           [(string? input) (string->bytes/utf-8 input)]
           [else (raise-argument-error 'parse-toml
                                       "input is not an input-port?, bytes?, or string?."
                                       input)])))
  (stx->dat (parse-result $toml-document
                          (open-input-bytes toml-bytes))))

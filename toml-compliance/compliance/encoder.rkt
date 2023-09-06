#!/usr/bin/env racket
#lang racket/base

(require gregor
         json
         racket/match
         toml)

(module+ main
  (displayln
   (tomlexpr->string
    (untype-jsexpr
     (read-json)))))

(define (untype-jsexpr js)
  (match js
    [(hash-table ('type "bool") ('value "true")) #t]
    [(hash-table ('type "bool") ('value "false")) #f]
    [(hash-table ('type "string") ('value v)) v]
    [(hash-table ('type "integer") ('value v)) (string->number v)]
    [(hash-table ('type "float") ('value "nan")) +nan.0]
    [(hash-table ('type "float") ('value "+nan")) +nan.0]
    [(hash-table ('type "float") ('value "-nan")) -nan.0]
    [(hash-table ('type "float") ('value "inf")) +inf.0]
    [(hash-table ('type "float") ('value "+inf")) +inf.0]
    [(hash-table ('type "float") ('value "-inf")) -inf.0]
    [(hash-table ('type "float") ('value "0")) 0.0]
    [(hash-table ('type "float") ('value v)) (string->number v)]
    [(hash-table ('type "datetime") ('value v)) (iso8601->moment v)]
    [(hash-table ('type "datetime-local") ('value v)) (iso8601->datetime v)]
    [(hash-table ('type "date-local") ('value v)) (iso8601->date v)]
    [(hash-table ('type "time-local") ('value v)) (iso8601->time v)]
    [(? list? v) (map untype-jsexpr v)]
    [(? hash? ht)
     (for/hasheq ([(k v) (in-hash ht)])
       (values k (untype-jsexpr v)))]))

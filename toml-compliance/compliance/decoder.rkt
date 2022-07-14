#!/usr/bin/env racket
#lang racket/base

(require json
         racket/date
         racket/format
         racket/match
         racket/port
         racket/list
         racket/math
         racket/string
         toml)

(module+ main
  (displayln
   (jsexpr->string
    (type-jsexpr
     (parse-toml (current-input-port))))))

(define (type-jsexpr v)
  (match v
    [#t (hasheq 'type "bool" 'value "true")]
    [#f (hasheq 'type "bool" 'value "false")]
    [(? string? v) (hasheq 'type "string" 'value v)]
    [(? exact-integer? v) (hasheq 'type "integer" 'value (~a v))]
    [(? real? v) (hasheq 'type "float"
                         'value (cond
                                  [(nan? v) "nan"]
                                  [(infinite? v) (string-replace (~a v) ".0" "")]
                                  [else (~a v)]))]
    [(? date? d) (hasheq 'type "datetime"
                         'value
                         (parameterize ([date-display-format 'iso-8601])
                           (string-append (date->string d #t) "Z")))]
    [(? list? xs)
     (for/list ([x xs])
       (type-jsexpr x))]
    [(? hash? ht) (for/hasheq ([(k v) (in-hash ht)])
                    (values k (type-jsexpr v)))]))

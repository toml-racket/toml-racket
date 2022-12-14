#!/usr/bin/env racket
#lang racket/base

(require gregor
         gregor/time
         json
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
    [(? moment? odt) (hasheq 'type "datetime"
                             'value (moment->iso8601 odt))]
    [(? datetime? ldt) (hasheq 'type "datetime-local"
                               'value (datetime->iso8601 ldt))]
    [(? date? ld) (hasheq 'type "date-local"
                          'value (date->iso8601 ld))]
    [(? time? lt) (hasheq 'type "time-local"
                          'value (time->iso8601 lt))]
    [(? list? xs)
     (for/list ([x xs])
       (type-jsexpr x))]
    [(? hash? ht) (for/hasheq ([(k v) (in-hash ht)])
                    (values k (type-jsexpr v)))]))

#lang racket/base

(require "misc.rkt"
         gregor
         gregor/time
         racket/format
         racket/list
         racket/match
         racket/string

         racket/contract)

(provide tomlexpr?

         (contract-out
          [tomlexpr->string
           (-> tomlexpr? any)])) ;; string?

(define (tomlexpr? x)
  (and (hash? x)
       (tomlval? x)))

(define (tomlval? x)
  (or (string? x)
      (number? x)
      (boolean? x)
      (moment? x)
      (datetime? x)
      (date? x)
      (time? x)
      (and (list? x) (andmap tomlval? x))
      (and (hash? x)
           (for/and ([(k v) (in-hash x)])
             (and (symbol? k)
                  (tomlval? v))))))

(module+ test
  (require rackunit)

  (check-false (tomlexpr? 5))
  (check-true (tomlexpr? '#hasheq((x . 0))))
  (check-true (tomlexpr? '#hash((xs . (1 2 3)))))
  (check-true (tomlexpr? `#hasheq((xs . (1 "b" #hash((noon . ,(time 12))))))))
  (check-false (tomlexpr? '#hash(("a" . "keys are symbols"))))
  (check-true (tomlexpr? '#hash((s . "string"))))
  (check-true (tomlexpr? '#hasheq((x . #t))))
  (check-true (tomlexpr? `#hasheq((date . #hash((moment . ,(moment 2023))))))))


(define (tomlexpr->string js)
  ;; HACK `tomlify` prefixes all tables with newline
  (string-trim (tomlify '() js)))

(define (tomlify root js [aot? #f] #:inline [inline? #f])
  (match js
    ['null ""]
    [#t "true"]
    [#f "false"]
    [(? string?) (~s js)]
    [+nan.0 "nan"]
    [-nan.0 "-nan"]
    [+inf.0 "inf"]
    [-inf.0 "-inf"]
    [(? real?) (~s js)]
    [(? moment? v) (moment->iso8601 v)]
    [(? datetime? v) (datetime->iso8601 v)]
    [(? date? v) (date->iso8601 v)]
    [(? time? v) (time->iso8601 v)]
    [(? list?)
     ;; Assumes array-of-tables case is covered already
     (for/fold ([str "[\n"]
                #:result (string-append str "]"))
               ([jse (in-list js)])
       ;; TODO fix indentation
       (string-append str
                      "  " (tomlify root jse #:inline #t) ",\n"))]
    [(? hash?)
     ;; Separate subtables from the other key/value pairs
     ;; HACK need vals to exclude aots as well--but doing that in the fold
     (define-values (hts vals)
       (partition (compose1 hash? cdr) (hash->list js)))
     ;; HACK For now, we're always sorting keys
     (define (sort-by-keys tb)
       (sort tb
             (λ (a b)
               (string<? (symbol->string (car a))
                         (symbol->string (car b))))))
     (define tbs
       (apply string-append
              (for/list ([ht (in-list (sort-by-keys hts))])
                (tomlify (cons (car ht) root)
                         (cdr ht)
                         #:inline inline?))))
     (for/fold ([kvs ""]
                [aots ""]
                #:result (cond
                           ;; Inline case
                           ;; Assumes we have correctly "inlined all the way"
                           [inline? (string-append "{ " (string-trim kvs ", ") " }")]
                           ;; Skip header case
                           ;; - no header for root
                           ;; - skip if there are no key/vals and another subtable or aot header
                           [(or (null? root)
                                (and (not aot?)
                                     (string=? "" kvs)
                                     (or (not (null? hts))
                                         (non-empty-string? aots))))
                            (string-append kvs tbs aots)]
                           [else (string-append "\n[" (if aot? "[" "")
                                                (keys->string (reverse root))
                                                "]" (if aot? "]" "") "\n"
                                                kvs
                                                tbs
                                                aots)]))
               ([k/v (in-list (sort-by-keys vals))])
       (match-define (cons key val) k/v)
       (define new-root (cons key root))
       ;; HACK Special case for arrays of tables
       (if (and (list? val)
                (for/and ([elt (in-list val)])
                  (hash? elt)))
           (values kvs
                   (apply string-append
                          (cons aots
                                (for/list ([ht (in-list val)])
                                  (tomlify new-root ht #t #:inline inline?)))))
           (values (string-append kvs
                                  (keys->string (list key))
                                  " = "
                                  (tomlify new-root val #:inline inline?)
                                  (if inline? ", " "\n"))
                   aots)))]
    [_ (error 'toml "couldn't print--whoops!")]))

#lang racket/base

(require racket/contract
         racket/string
         racket/function
         racket/match
         json
         "stx.rkt")

(provide (all-defined-out))

;;; hasheq-merge

;; Merge two hasheq's h0 and h1.
;;
;; When a key exists in only one, use its value.
;;
;; When a key exists in both, when the values are
;;  - both hasheqs? do a recursive hasheq-merge
;;  - both lists? append the lists
;;   - otherwise raise an error.
(define/contract (hasheq-merge h0 h1 [keys '()])
  (->* ((and/c immutable? hash?) (and/c immutable? hash?))
       ((listof symbol?))
       (and/c immutable? hash?))
  (for/fold ([h0 h0])
            ([(k v1) h1])
    (hash-set h0 k
              (cond [(list? v1) ;; array of tables
                     (define v0 (hash-ref h0 k (list)))
                     (unless (list? v0)
                       (conflict-error (cons k keys) v0 v1))
                     (append v0 v1)]
                    [(hash? v1)
                     (define v0 (hash-ref h0 k (hasheq)))
                     (unless (hash? v0)
                       (conflict-error (cons k keys) v0 v1))
                     (hasheq-merge v1 v0 (cons k keys))]
                    [(hash-has-key? h0 k)
                     (conflict-error (cons k keys) (hash-ref h0 k) v1)]
                    [else v1]))))

(define (conflict-error ks v0 v1)
  (error 'toml
         "conflicting values for `~a'\n at ~a: `~a'\n at ~a: `~a'"
         (string-join (map symbol->string (reverse ks)) ".")
         (find-pos v0) (jsexpr->string (stx->dat v0))
         (find-pos v1) (jsexpr->string (stx->dat v1))))

(module+ test
  (check-equal?
   (hasheq-merge (hasheq 'foo "bar"
                         'bar "baz"
                         'baz (hasheq 'a "a")
                         'xs (list (hasheq 'x0 10 'x1 11)))
                 (hasheq 'a "a"
                         'baz (hasheq 'b "b")
                         'xs (list (hasheq 'x0 20 'x1 21))))
   (hasheq 'foo "bar"
           'bar "baz"
           'a "a"
           'baz (hasheq 'a "a"
                        'b "b")
           'xs (list (hasheq 'x0 10 'x1 11)
                     (hasheq 'x0 20 'x1 21))))
  (check-exn #rx"conflicting values for `a.b.c'"
             (λ ()
               (hasheq-merge
                (hasheq 'a (hasheq 'b (hasheq 'c 0)))
                (hasheq 'a (hasheq 'b (hasheq 'c 1)))))))

;;; misc utils

(define (merge hts keys) ;; (listof hasheq?) (listof symbol?) -> hasheq?
  (catch-redefs hts) ;; WHY?? Won't hasheq-merge catch this ???
  (foldl (curryr hasheq-merge keys) (hasheq) hts))

(define (catch-redefs hts)
  (let loop ([hts hts])
    (match hts
      [(cons ht0 more)
       (for ([ht1 (in-list more)])
         (when (equal? ht0 ht1)
           (error 'toml
                  "redefinition of `~a'"
                  (keys->string (ht->keys ht0)))))
       (loop more)]
      [_ (void)])))

(define (ht->keys ht)
  (match ht
    [(hash-table (k v)) (cons k (ht->keys v))]
    [_ '()]))

(define (keys->string ks)
  (string-join (map symbol->string ks) "."))

(define (kvs->hasheq keys pairs [orig-keys keys])
  ;; (listof symbol?) (listof (list/c symbol? any/c)) -> hasheq?
  (match keys
    [(list* this more) (hasheq this (kvs->hasheq more pairs orig-keys))]
    [(list) (for/fold ([ht (hasheq)])
                      ([p (in-list pairs)])
              (match-define (list k v) p)
              (define relative (match k [(? symbol? s) (list s)] [z z]))
              (define start (append relative (reverse orig-keys)) )
              (define (place ht keypath)
                (match keypath
                  [(list sym)
                   (when (hash-has-key? ht sym)
                     (conflict-error start (hash-ref ht sym) v))
                   (hash-set ht sym v)]
                  [(list k0 krest ...)
                   (when (and (hash-has-key? ht k0))
                     (define dest (hash-ref ht k0))
                     (unless (hash? dest)
                       (error 'toml
                              "redefinition of `~a`"
                              (keys->string start))))
                   (hash-update ht k0 (curryr place krest) (const (hasheq)))]))
              (place ht relative))]))

(module+ test
  (check-exn #rx"conflicting values for `a.b.c.x'"
             (λ () (kvs->hasheq '(a b c) '([x 0][x 1]))))
  (check-equal? (kvs->hasheq '() '([x 0][y 1]))
                (hasheq 'x 0 'y 1))
  (check-equal? (kvs->hasheq '(a) '([x 0][y 1]))
                (hasheq 'a (hasheq 'x 0 'y 1)))
  (check-equal? (kvs->hasheq '(a b) '([x 0][y 1]))
                (hasheq 'a (hasheq 'b (hasheq 'x 0 'y 1))))
  (check-equal? (kvs->hasheq '(a) '())
                (hasheq 'a (hasheq))))

(define (hash-refs ht keys)
  (match keys
    [(list)         ht]
    [(list* k more) (hash-refs (hash-ref ht k) more)]))

(module+ test
  (require rackunit)
  (check-equal? (hash-refs #hasheq([a . 0]) '())
                #hasheq([a . 0]))
  (check-equal? (hash-refs #hasheq([a . 0]) '(a))
                0)
  (check-equal? (hash-refs #hasheq([a . #hasheq([b . 0])]) '(a b))
                0)
  (check-equal? (hash-refs #hasheq([a . #hasheq([b . #hasheq([c . 0])])]) '(a b c))
                0))

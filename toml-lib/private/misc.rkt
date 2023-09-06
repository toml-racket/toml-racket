#lang racket/base

(require racket/contract
         racket/set
         racket/string
         racket/function
         racket/match
         json
         "stx.rkt")

(provide merge
         keys->string
         kvs->table
         kvs->hasheq
         change-root)

(module+ test
  (require rackunit))

;; The `table` struct contains both the TOML contents and metadata required to check for duplicates.
;;
;; header is a list of symbols based on header-specified keys, or #f if an array of tables.
;; implied is a list of keys (symbol lists) implied by dotted keys in key/value pairs of a table.
;; contents is the full document's hash table restricted to the specified table section, not
;;   merely a subtree rooted at the header node. This was based on the existing parsing approach;
;;   a investigation/comparison with alternative approaches has not been done.
(struct table (header implied contents) #:transparent)

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

;; Raises an error for conflicting values.
;; Note that `ks` is the keypath of the conflict in reverse, easing the recursive `hasheq-merge`.
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
;; Returns hasheq resulting from the combination of the tables.
;; Checks for duplicate headers via `consolidate-headers` and overlap with dotted keys.
(define (merge tbs keys) ;; (listof table?) (listof symbol?) -> hasheq?
  (define headers (consolidate-headers tbs keys))
  (for/fold ([ht (hasheq)])
            ([tb (in-list tbs)])
    (define overlap (set-intersect headers (table-implied tb)))
    (unless (set-empty? overlap)
      (error 'toml "redefinition of `~a' with dotted key" (keys->string (set-first overlap))))
    (hasheq-merge (table-contents tb) ht keys)))

;; Combines tables that are not part of arrays and raises errors for duplicates.
(define (consolidate-headers tbs keys)
  (for*/fold ([names (set)])
             ([tb (in-list tbs)]
              [keys (in-value (table-header tb))]
              #:when keys)
    (when (set-member? names keys)
      (error 'toml "redefinition of `~a'" (keys->string keys)))
    (set-add names keys)))

;; Converts lists of keys (symbols) to the corresponding dotted string for TOML headers.
;; TOML specifies that all Unicode characters are allowed except \u0000-\u0008, \u000A-\u001F,
;; and \u007F. Unescaped tab (\u0009) is permitted, but TOML does not appear to prohibit
;; using the escaped form, either.
(define (keys->string ks)
  (define (control-escapes s)
    (match s
      ["\b" "\\b"]
      ["\n" "\\n"]
      ["\f" "\\f"]
      ["\r" "\\r"]
      ["\t" "\\t"]
      ["\u007F" "\\u007f"]
      [other-control
       (define n (char->integer (string-ref other-control 0)))
       (string-append "\\u"
                      (if (< n 16) "000" "00")
                      (number->string n 16))]))
  (string-join (for/list ([sym (in-list ks)])
                 (define str
                   (regexp-replace* #rx"[\u0000-\u001F\u007F]"
                                    (symbol->string sym)
                                    control-escapes))
                 (if (regexp-match? #rx"^[A-Za-z0-9_-]+$" str)
                     str
                     (if (string-contains? str "\"")
                         (string-append "'" str "'")
                         (string-append "\"" str "\""))))
               "."))

(module+ test
  (check-equal? (keys->string '()) "")
  (check-equal? (keys->string '(a b)) "a.b")
  (check-equal? (keys->string '(a b.c)) "a.\"b.c\"")
  (check-equal? (keys->string (list (string->symbol "backsp\b\b"))) "\"backsp\\b\\b\"")
  (check-equal? (keys->string (list (string->symbol "\u001b"))) "\"\\u001b\"")
  (check-equal? (keys->string (list (string->symbol "\u007f"))) "\"\\u007f\"")
  (check-equal? (keys->string '(|"123"|)) "'\"123\"'"))

(define (kvs->table keys pairs [aot? #f] [orig-keys keys])
  ;; (listof symbol?) (listof (list/c (listof symbol?) any/c)) -> table?
  (match keys
    [(list* this more)
     (define embedded (kvs->table more pairs aot? orig-keys))
     (struct-copy table embedded
                  [contents (hasheq this (table-contents embedded))])]
    [(list) (for/fold ([ht (table (and (not aot?) orig-keys) (set) (hasheq))]
                       #:result (struct-copy table ht
                                             [implied (set-remove (table-implied ht) orig-keys)]))
                      ([p (in-list pairs)])
              (match-define (list ks v) p)
              (define (place tb parents keypath)
                (match-define (table names dotted ht) tb)
                (match keypath
                  [(list sym)
                   (when (hash-has-key? ht sym)
                     (conflict-error (cons sym parents) (hash-ref ht sym) v))
                   (struct-copy table tb
                                [contents (hash-set ht sym v)])]
                  [(list k0 krest ...)
                   (define base-keys (reverse (cons k0 parents)))
                   (when (hash-has-key? ht k0)
                     (define dest (hash-ref ht k0))
                     (unless (hash? dest)
                       (error 'toml
                              "redefinition of `~a`" ; TODO inconsistent terminal character?
                              (keys->string base-keys))))
                   (define child
                     (place (table names dotted (hash-ref ht k0 (const (hasheq))))
                            (cons k0 parents)
                            krest))
                   (struct-copy table tb
                                [implied (set-add (table-implied child) base-keys)]
                                [contents (hash-set ht k0 (table-contents child))])]))
              (place ht (reverse orig-keys) ks))]))

(define (kvs->hasheq kvs)
  (table-contents (kvs->table '() kvs)))

(module+ test
  (check-exn #rx"conflicting values for `a.b.c.x'"
             (λ () (kvs->table '(a b c) '([(x) 0][(x) 1]))))
  (check-equal? (kvs->table '() '([(x) 0][(y) 1]))
                (table '() (set) (hasheq 'x 0 'y 1)))
  (check-equal? (kvs->table '(a) '([(x) 0][(y) 1]))
                (table '(a) (set) (hasheq 'a (hasheq 'x 0 'y 1))))
  (check-equal? (kvs->table '(a) '([(x) 0][(y) 1]) #t)
                (table #f (set) (hasheq 'a (hasheq 'x 0 'y 1))))
  (check-equal? (kvs->table '(a b) '([(x) 0][(y) 1]))
                (table '(a b) (set) (hasheq 'a (hasheq 'b (hasheq 'x 0 'y 1)))))
  (check-equal? (kvs->table '(a) '())
                (table '(a) (set) (hasheq 'a (hasheq))))
  (check-equal? (kvs->table '(a) '([(b x) 0][(b y) 1]))
                (table '(a) (set '(a b)) (hasheq 'a (hasheq 'b (hasheq 'x 0 'y 1)))))
  (check-equal? (kvs->table '(a) '([(b c t) 9]))
                (table '(a) (set '(a b) '(a b c)) (hasheq 'a (hasheq 'b (hasheq 'c (hasheq 't 9)))))))

(define (hash-refs ht keys)
  (match keys
    [(list)         ht]
    [(list* k more) (hash-refs (hash-ref ht k) more)]))

(module+ test
  (check-equal? (hash-refs #hasheq([a . 0]) '())
                #hasheq([a . 0]))
  (check-equal? (hash-refs #hasheq([a . 0]) '(a))
                0)
  (check-equal? (hash-refs #hasheq([a . #hasheq([b . 0])]) '(a b))
                0)
  (check-equal? (hash-refs #hasheq([a . #hasheq([b . #hasheq([c . 0])])]) '(a b c))
                0))

(define (change-root tb keys)
  (struct-copy table tb
               [contents (hash-refs (table-contents tb) keys)]))

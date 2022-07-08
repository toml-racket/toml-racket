#lang racket/base

(require racket/match
         racket/format)

(provide (all-defined-out))

;;; stx

;; Parsac automatically provides error messages with positions for
;; _syntax_ errors. To do so also for _semantic_ errors -- e.g. hash
;; conflicts -- we need to tag the source datums with pos info. Much
;; like Racket syntax objects. Unlike Racket syntax, it may be
;; sufficient for us to tag only _some_ of the input, such as
;; $key/val, for adequate error messages.

(struct stx (e pos) #:transparent)

;; Strip all stx structs rescursively. Analogous to Racket's
;; `syntax->datum`.
(define (stx->dat v)
  (match v
    [(? hash? ht) (for/hasheq ([(k v) (in-hash ht)]) (values k (stx->dat v)))]
    [(? list? xs) (for/list ([x (in-list xs)]) (stx->dat x))]
    [(stx e _)    e]
    [v            v]))

;; Depth-first search for the first value that's a stx? and return its
;; pos converted to a line:col:ofs string, or #f if none found.
(define (find-pos v)
  (match v
    [(? hash? ht) (for/or ([(k v) (in-hash ht)]) (find-pos v))]
    [(? list? xs) (for/or ([x (in-list xs)]) (find-pos x))]
    [(stx _ (list r c pos)) (~a r ":" c)]
    [v #f]))

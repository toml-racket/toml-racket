#lang at-exp racket/base


(require racket/function
         racket/format

         "../parsers.rkt"
         "../parsack.rkt")

(module+ test
  (require rackunit)
  (test-equal? "Table with spaces between brackets"
               (parse-toml @~a{[ a ]
                               x = 1})
               #hasheq((a . #hasheq((x . 1))))))

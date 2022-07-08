#lang at-exp racket/base


(require racket/function
         racket/format

         "../parsack.rkt"
         "../parsers/main.rkt")

(module+ test
  (require rackunit)
  (test-equal? "Table with spaces between brackets"
               (parse-toml @~a{[ a ]
                               x = 1})
               #hasheq((a . #hasheq((x . 1))))))

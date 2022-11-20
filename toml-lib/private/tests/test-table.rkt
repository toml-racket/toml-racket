#lang at-exp racket/base


(require racket/function
         racket/format

         "../parsack.rkt"
         "../parsers/main.rkt")

(module+ test
  (require rackunit)

  (test-exn "No line between table header and key"
            exn:fail:parsack?
            (thunk (parse-toml @~a{[header] key = "can't be same line"})))

  (test-exn "No line between array table header and key"
            exn:fail:parsack?
            (thunk (parse-toml @~a{[[header]] key = "can't be same line"})))

  ; Consider moving. Not really a table issue, but related to the above tests.
  (test-exn "Keys need newlines"
            exn:fail:parsack?
            (thunk (parse-toml @~a{keys = "need" new = "lines"})))

  (test-equal? "Table with spaces between brackets"
               (parse-toml @~a{[ a ]
                               x = 1})
               #hasheq((a . #hasheq((x . 1))))))

#lang at-exp racket/base


(require racket/function

         "../parsers.rkt"
         "../parsack.rkt")

(module+ test
  (require rackunit)

  (test-equal? "Oct Lit TOML 0o01234567"
               (parse-toml "oct1 = 0o01234567")
               #hasheq((oct1 . #o1234567)))

  (test-equal? "Bin Lit TOML (1010)"
               (parse-toml "x = 0b1010")
               #hasheq((x . #b1010)))

  (test-equal? "Neg Dec Lit TOML (-1_2_345)"
               (parse-toml "x = -1_2_345")
               #hasheq((x . -12345)))

  (test-equal? "Hex Lit TOML (0xdeadbeef"
               (parse-toml "x = 0xdead_beef")
               #hasheq((x .  #xdeadbeef)))

  (test-exn "BAD Dec Lit TOML (123_)"
            #rx""
            (thunk (parse-toml "x = 123_"))))

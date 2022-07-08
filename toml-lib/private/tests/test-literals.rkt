#lang at-exp racket/base


(require racket/function
         racket/format

         "../parsack.rkt"

         "../parsers/main.rkt")


(module+ test
  (require rackunit)

  (test-equal? "$exp parses e-12"
               (parse-result $exp "e-12")
               -12)

  (test-equal? "$float parses 1.2345"
               (parse-result $float "1.2345")
               1.2345)

  (test-equal? "$val parses 1.2345"
               (parse-result $float "1.2345")
               1.2345)

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
            (thunk (parse-toml "x = 123_")))

  (test-equal? "Lit. float -inf"
               (parse-toml "x = -inf")
               #hasheq((x . -inf.0)))

  (test-equal? "Lit. float inf"
               (parse-toml "x = inf")
               #hasheq((x . +inf.0)))

  (test-equal? "Lit. float nan"
               (parse-toml "x = nan")
               #hasheq((x . +nan.0)))

  (test-equal? "Lit. float 1.2345"
               (parse-toml "x = 1.2345")
               #hasheq((x . 1.2345)))

  (test-equal? "Lit. float 1e5"
               (parse-toml "x = 1e5")
               #hasheq((x . 1e5)))

  (test-equal? "Lit. float 1.23e5"
               (parse-toml "x = 1.23e5")
               #hasheq((x . 1.23e5)))

  (test-equal? "Lit. float 1.23e-5"
               (parse-toml "x = 1.23e-5")
               #hasheq((x . 1.23e-5)))

  (test-equal? "Lit. Str 'foo bar baz'"
               (parse-toml "x = 'foo bar baz'")
               #hasheq((x . "foo bar baz")))

  (test-equal? "Escaped Str \"foo bar baz\""
               (parse-toml "x = \"foo bar baz\"")
               #hasheq((x . "foo bar baz")))

  (test-equal? "Empty array"
               (parse-toml "x = [[[]]]")
               #hasheq((x . [[[]]])))

  #;
  (test-equal? "Array"
               (parse-toml "x = [1,2,[true,false]]")
               #hasheq((x . [1 2 [#t #f]])))

  (test-equal? "ar0 = [1,2,3]"
               (parse-toml "ar0 = [1,2,3]")
                #hasheq((ar0 . (1 2 3))))

  (test-equal? "ar0 = [ 1, 2, 3] "
                (parse-toml "ar0 = [ 1, 2, 3] ")
                #hasheq((ar0 . (1 2 3))))

  (test-equal? "Parse empty array w whitespace inside"
               (parse-toml "ar0 = [ ]")
               #hasheq((ar0 . ())))

  (test-equal? "defining a single-array with newlines"
               (parse-toml @~a{array2 = [
                                         1
                                         ]})
               #hasheq((array2 . (1))))

  (test-equal? "Parse array literal with trailingcomma"
               (parse-toml "array2 = [1,]")
               #hasheq((array2 . (1))))

  (test-equal? "Parse array literal with comment after trailingcomma"
               (parse-toml @~a{
                               array2 = [
                                         1, # test
                                         ]
                               })
               #hasheq((array2 . (1))))

  (test-equal? "Parse Array literal with comment after first elem"
               (parse-toml @~a{
                               array2 = [
                                         1, # comment
                                         2,
                                         3,
                                         ]
                               })
               #hasheq((array2 . (1 2 3))))

  (test-equal?
   "Parse many comments within an array literal"
   (parse-toml @~a{
                   array2 = [ #comment
                              1, #comment
                              2,
                              3,
                              ] #comment
                   })
   #hasheq((array2 . (1 2 3)))))

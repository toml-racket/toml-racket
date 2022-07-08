#lang at-exp racket/base

(require racket/function
         json

         "../parsack.rkt"
         "../../main.rkt")

(module+ test
  (require rackunit
           racket/format)
  (check-equal? (parse-toml @~a{[a]})
                '#hasheq((a . #hasheq())))
  (check-equal? (parse-toml @~a{[a.b]})
                '#hasheq((a . #hasheq((b . #hasheq())))))
  (check-equal? (parse-toml @~a{today = 2014-06-26T12:34:56Z})
                `#hasheq((today . ,(date 56 34 12 26 6 2014 0 0 #f 0))))
  ;; toml-tests: `duplicate-keys`
  (check-exn #rx"conflicting values for `x'"
             (λ () (parse-toml @~a{x=1
                                   x=2})))
  ;; toml-tests: `duplicate-tables`
  (check-exn #rx"redefinition of `a'"
             (λ () (parse-toml @~a{[a]
                                   [a]})))
  ;; toml-tests: table-sub-empty
  (check-equal? (parse-toml @~a{[a]
                                [a.b]})
                '#hasheq((a . #hasheq((b . #hasheq())))))
  ;; My own test for duplicate tables
  (check-exn #rx"redefinition of `a.b'"
             (λ () (parse-toml @~a{[a.b]
                                   [a.C]
                                   [a.b]
                                   })))
  (check-exn #rx"redefinition of `a'"
             (λ () (parse-toml @~a{[a]
                                   [b]
                                   [a]
                                   })))
  ;; README examples
  (check-equal? (parse-toml @~a{[a.b]
                                c = 1
                                [a]
                                d = 2})
                '#hasheq((a . #hasheq((b . #hasheq((c . 1)))
                                      (d . 2)))))
  #;
  (check-exn #rx"redefinition of `a'"
             (λ () (parse-toml @~a{[a]
                                   b = 1
                                   [a]
                                   c = 2})))
  (check-exn #rx"conflicting values for `a.b'"
             (λ () (parse-toml @~a{[a]
                                   b = 1
                                   [a.b]
                                   c = 2})))
  (check-exn exn:fail:parsack? (λ () (parse-toml "[]")))
  (check-exn exn:fail:parsack? (λ () (parse-toml "[a.]")))
  (check-exn exn:fail:parsack? (λ () (parse-toml "[a..b]")))
  (check-exn exn:fail:parsack? (λ () (parse-toml "[.b]")))
  (check-exn exn:fail:parsack? (λ () (parse-toml "[.]")))
  (check-exn exn:fail:parsack? (λ () (parse-toml " = 0")))
  (check-equal?
   (parse-toml @~a{[[aot.sub]] #comment
                   aot0 = 10
                   aot1 = 11

                   [[aot.sub]] #comment
                   aot0 = 20
                   aot1 = 21

                   [[aot.sub]] #comment
                   aot0 = 30
                   aot1 = 31
                   })
   '#hasheq((aot
             .
             #hasheq((sub
                      .
                      (#hasheq((aot0 . 10) (aot1 . 11))
                       #hasheq((aot0 . 20) (aot1 . 21))
                       #hasheq((aot0 . 30) (aot1 . 31))))))))

  (test-equal?
   "Parse a long toml document"
   (parse-toml @~a{# Comment blah blah
                   # Comment blah blah

                   foo = "bar" #comment
                   ten = 10
                   true = true
                   false = false
                   array0 = [1,2,3] #comment
                   array1 = [ 1, 2, 3, ]
                   array2 = [ #comment
                              1, #comment
                              2,
                              3,
                              ] #comment
                   nested-array = [[1, 2, 3], [4, 5, 6]] #comment

                   [key0.key1] #comment
                   x = 1
                   y = 1
                   [key0.key2]
                   x = 1
                   y = 1

                   [[aot.sub]] #comment
                   aot0 = 10
                   aot1 = 11

                   [[aot.sub]] #comment
                   aot0 = 20
                   aot1 = 21

                   [[aot.sub]] #comment
                   aot0 = 30
                   aot1 = 31
                   })
   '#hasheq((foo . "bar")
            (false . #f)
            (true . #t)
            (aot
             .
             #hasheq((sub
                      .
                      (#hasheq((aot0 . 10) (aot1 . 11))
                       #hasheq((aot0 . 20) (aot1 . 21))
                       #hasheq((aot0 . 30) (aot1 . 31))))))
            (ten . 10)
            (array0 . (1 2 3))
            (array1 . (1 2 3))
            (array2 . (1 2 3))
            (nested-array . ((1 2 3) (4 5 6)))
            (key0
             .
             #hasheq((key1 . #hasheq((x . 1) (y . 1)))
                     (key2 . #hasheq((x . 1) (y . 1)))))))
  (check-equal?
   (parse-toml @~a{[[fruit]]
                   name = "apple"

                   [fruit.physical]
                   color = "red"
                   shape = "round"

                   [[fruit]]
                   name = "banana"
                   })
   '#hasheq((fruit
             .
             (#hasheq((name . "apple")
                      (physical
                       .
                       #hasheq((color . "red") (shape . "round"))))
              #hasheq((name . "banana"))))))
  ;; From TOML README
  (check-equal?
   (parse-toml @~a{[[fruit]]
                   name = "apple"

                   [fruit.physical]
                   color = "red"
                   shape = "round"

                   [[fruit.variety]]
                   name = "red delicious"

                   [[fruit.variety]]
                   name = "granny smith"

                   [[fruit]]
                   name = "banana"

                   [[fruit.variety]]
                   name = "plantain"
                   })
   '#hasheq((fruit
             .
             (#hasheq((name . "apple")
                      (physical
                       .
                       #hasheq((color . "red") (shape . "round")))
                      (variety
                       .
                       (#hasheq((name . "red delicious"))
                        #hasheq((name . "granny smith")))))
              #hasheq((name . "banana")
                      (variety
                       .
                       (#hasheq((name . "plantain")))))))))
  ;; https://github.com/toml-lang/toml/issues/214
  (check-equal?
   (parse-toml @~a{[[foo.bar]]})
   (parse-toml @~a{[foo]
                   [[foo.bar]]}))
  ;; example from TOML README
  (check-exn
   #rx"conflicting values for `fruit.variety'"
   (λ () (parse-toml @~a{# INVALID TOML DOC
                         [[fruit]]
                         name = "apple"

                         [[fruit.variety]]
                         name = "red delicious"

                         # This table conflicts with the previous table
                         [fruit.variety]
                         name = "granny smith"})))
  ;; https://github.com/toml-lang/toml/pull/199#issuecomment-47300021
  ;; The tables and arrays of tables may come in ANY order. A plain table
  ;; may come "in the middle" of a nested table definition.
  (check-equal?
   (parse-toml @~a{[table]
                   key = 5

                   [[table.array]]
                   a = 1
                   b = 2

                   [another-table]
                   key = 10

                   [[table.array]]
                   a = 2
                   b = 4})
   #hasheq((|another-table| . #hasheq((key . 10)))
           (table . #hasheq((key . 5)
                            (array . (#hasheq((a . 1) (b . 2))
                                      #hasheq((a . 2) (b . 4))))))))

  (check-exn #rx""
             (λ () (parse-toml @~a{
                                   [a#b]
                                   x=1
                                   }))
             "Invalid character in table name")

  (check-equal?
   (parse-toml @~a{a.b.c = true})
   #hasheq((a . #hasheq((b . #hasheq((c . #t)))))))

  (check-exn #rx""
             (thunk
              (parse-toml @~a{
                              x = [1 2 3]
                              })))

  (test-equal?
   "Empty document is valid TOML"
   (parse-toml "")
   #hasheq())

  #;
  (check-exn #rx""
             (thunk (parse-toml @~a{
                                    [a]
                                    b = 1
                                    [a]
                                    c = 2
                                    }))))

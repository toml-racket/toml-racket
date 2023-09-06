#lang at-exp racket/base

(require gregor

         "../../main.rkt")

(module+ test
  (require rackunit
           racket/format)

  ;; Tests based on parse-toml tests
  (check-equal? (tomlexpr->string '#hasheq((a . #hasheq())))
                "[a]")
  (check-equal? (tomlexpr->string '#hasheq((a . #hasheq((b . #hasheq())))))
                "[a.b]")
  (check-equal? (tomlexpr->string `#hasheq((today . ,(moment 2014 6 26 12 34 56 #:tz 0))))
                "today = 2014-06-26T12:34:56Z")
  ;; README examples
  (check-equal? (tomlexpr->string '#hasheq((a . #hasheq((b . #hasheq((c . 1))) (d . 2)))))
                @~a{[a]
                    d = 2

                    [a.b]
                    c = 1})
  ;; TOML v1.0.0 table examples
  (test-equal? "Nested tables"
               (tomlexpr->string '#hasheq((fruit
                               .
                               #hasheq((apple
                                        .
                                        #hasheq((color . "red")
                                                (taste . #hasheq((sweet . #t)))
                                                (texture . #hasheq((smooth . #t)))))))))
               @~a{[fruit.apple]
                   color = "red"

                   [fruit.apple.taste]
                   sweet = true

                   [fruit.apple.texture]
                   smooth = true})
  (check-equal?
   (tomlexpr->string '#hasheq((aot
                   .
                   #hasheq((sub
                            .
                            (#hasheq((aot0 . 10) (aot1 . 11))
                             #hasheq((aot0 . 20) (aot1 . 21))
                             #hasheq((aot0 . 30) (aot1 . 31))))))))
   @~a{[[aot.sub]]
       aot0 = 10
       aot1 = 11

       [[aot.sub]]
       aot0 = 20
       aot1 = 21

       [[aot.sub]]
       aot0 = 30
       aot1 = 31})

  (test-equal?
   "Parse a long TOML document" ;; TODO clean up indentation
   (tomlexpr->string
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
   @~a{array0 = [
         1,
         2,
         3,
       ]
       array1 = [
         1,
         2,
         3,
       ]
       array2 = [
         1,
         2,
         3,
       ]
       false = false
       foo = "bar"
       nested-array = [
         [
         1,
         2,
         3,
       ],
         [
         4,
         5,
         6,
       ],
       ]
       ten = 10
       true = true

       [[aot.sub]]
       aot0 = 10
       aot1 = 11

       [[aot.sub]]
       aot0 = 20
       aot1 = 21

       [[aot.sub]]
       aot0 = 30
       aot1 = 31

       [key0.key1]
       x = 1
       y = 1

       [key0.key2]
       x = 1
       y = 1})
  (check-equal?
   (tomlexpr->string '#hasheq((fruit
                               .
                               (#hasheq((name . "apple")
                                        (physical
                                         .
                                         #hasheq((color . "red") (shape . "round"))))
                                #hasheq((name . "banana"))))))
   @~a{[[fruit]]
       name = "apple"

       [fruit.physical]
       color = "red"
       shape = "round"

       [[fruit]]
       name = "banana"})
  ;; From TOML README
  (check-equal?
   (tomlexpr->string '#hasheq((fruit
                               .
                               (#hasheq((name . "apple")
                                        (physical . #hasheq((color . "red") (shape . "round")))
                                        (variety . (#hasheq((name . "red delicious"))
                                                    #hasheq((name . "granny smith")))))
                                #hasheq((name . "banana")
                                        (variety . (#hasheq((name . "plantain")))))))))
   @~a{[[fruit]]
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
       name = "plantain"})
  ;; https://github.com/toml-lang/toml/pull/199#issuecomment-47300021
  ;; The tables and arrays of tables may come in ANY order. A plain table
  ;; may come "in the middle" of a nested table definition.
  (check-equal?
   (tomlexpr->string '#hasheq((|another-table| . #hasheq((key . 10)))
                              (table . #hasheq((key . 5)
                                               (array . (#hasheq((a . 1) (b . 2))
                                                         #hasheq((a . 2) (b . 4))))))))
   @~a{[another-table]
       key = 10

       [table]
       key = 5

       [[table.array]]
       a = 1
       b = 2

       [[table.array]]
       a = 2
       b = 4})
  (check-equal?
   (tomlexpr->string #hasheq((a . #hasheq((b . #hasheq((c . #t)))))))
   @~a{[a.b]
       c = true})

  ;; Floating point special cases
  (test-equal? "Floating point special cases"
               (tomlexpr->string '#hasheq((nan . +nan.0)
                                          (infinity . +inf.0)
                                          (infinity_neg . -inf.0)
                                          (float_zero . 0.0)))
               @~a{float_zero = 0.0
                   infinity = inf
                   infinity_neg = -inf
                   nan = nan})

  (test-equal? "Array with string and table - needs to inline"
               (tomlexpr->string '#hasheq((contributors
                                           .
                                           ("Foo Bar <foo@example.com>"
                                            #hasheq((name . "Baz Qux")
                                                    (email . "bazqux@example.com")
                                                    (url . "https://example.com/bazqux"))))))
               @~a|{contributors = [
                      "Foo Bar <foo@example.com>",
                      { email = "bazqux@example.com", name = "Baz Qux", url = "https://example.com/bazqux" },
                    ]}|)

  ;; Quoted-key handling
  (test-equal? "Empty quoted key"
               (tomlexpr->string '#hasheq((|| . 23)))
               @~a{"" = 23})
  (test-equal? "Quotes within quotes"
               (tomlexpr->string
                '#hasheq((a . #hasheq((|"b"| . #hasheq((c . #hasheq((answer . 42)))))))))
               @~a{[a.'"b"'.c]
                   answer = 42})
  (test-equal?
   "Dotted keys, not subtables"
   (tomlexpr->string '#hasheq((plain . 1)
                              (plain_table . #hasheq((plain . 3) (with.dot . 4)))
                              (table . #hasheq((withdot
                                                .
                                                #hasheq((key.with.dots . 6) (plain . 5)))))
                              (with.dot . 2)))
   @~a{plain = 1
       "with.dot" = 2

       [plain_table]
       plain = 3
       "with.dot" = 4

       [table.withdot]
       "key.with.dots" = 6
       plain = 5})
  (test-equal? "Pound in table header"
               (tomlexpr->string '#hasheq((key#group . #hasheq((answer . 42)))))
               @~a{["key#group"]
                   answer = 42})
  (test-equal? "Strings/quoted keys cannot contain control characters"
               (tomlexpr->string '#hasheq((|
| . "newline") (backsp . #hasheq())))
               @~a{"\n" = "newline"

                   ["backsp\b\b"]})

  ;; Arrays of Tables
  (test-equal? "Subtables of arrays of tables"
               (tomlexpr->string '#hasheq((arr
                                           .
                                           (#hasheq((a . #hasheq((b . #hasheq((c . 1) (d . 2))))))
                                            #hasheq((a . #hasheq((b . #hasheq((c . 3) (d . 4))))))))))
               @~a{[[arr]]

                   [arr.a.b]
                   c = 1
                   d = 2

                   [[arr]]

                   [arr.a.b]
                   c = 3
                   d = 4})
  )

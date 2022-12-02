#lang at-exp racket/base


(require gregor
         gregor/time
         racket/function
         racket/format

         "../parsack.rkt"

         "../parsers/main.rkt")


(module+ test
  (require rackunit)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Float
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-equal? "$exp parses e-12"
               (parse-result $exp "e-12")
               -12)

  (test-equal? "$float parses 1.2345"
               (parse-result $float "1.2345")
               1.2345)

  (test-equal? "$val parses 1.2345"
               (parse-result $float "1.2345")
               1.2345)
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

  (test-equal? "Lit. float 1E5"
               (parse-toml "x = 1E5")
               #hasheq((x . 1E5)))

  (test-equal? "Lit. float 1.23e5"
               (parse-toml "x = 1.23e5")
               #hasheq((x . 1.23e5)))

  (test-equal? "Lit. float 1.23e-5"
               (parse-toml "x = 1.23e-5")
               #hasheq((x . 1.23e-5)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Integer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; String
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-exn "basic Str does not accept 3 hexdigit unicode escape"
            #rx""
            (thunk (parse-toml "x = \"\\u0d8\"")))

  (test-equal? "basic Str Accepts 4 hexdigit unicode escape"
               (parse-toml "x = \"\\u00d8\"")
               #hasheq((x . "Ø")))

  (test-equal? "basic Str Accepts 8 hexdigit unicode escape"
               (parse-toml "x = \"\\u000000d8\"")
               #hasheq((x . "Ø")))

  (test-equal? "Escaped Str \"foo bar baz\""
               (parse-toml "x = \"foo bar baz\"")
               #hasheq((x . "foo bar baz")))

  (test-equal? "Lit. Str 'foo bar baz'"
               (parse-toml "x = 'foo bar baz'")
               #hasheq((x . "foo bar baz")))

  (test-exn "Bad parse Lit. Str 'foo bar baz'"
            #rx""
            (thunk
             (parse-toml "x = 'foo\r\nbar\nbaz'")))

  (test-exn "Bad parse Str \"foo bar baz\""
            #rx""
            (thunk
             (parse-toml "x = \"foo\r\nbar\nbaz\"")))

  (test-equal? "multiline string parses"
               (parse-toml "x = \"\"\"hello\nworld\"\"\"")
               #hasheq((x . "hello\nworld")))

  (test-equal? "multiline string opener eats following newline"
               (parse-toml "x = \"\"\"\nhello\nworld\"\"\"")
               #hasheq((x . "hello\nworld")))

  (test-equal? "Multiline basic string"
               (parse-toml #<<END
x = """
hello
world
"""
END
                           )
               `#hasheq((x . , @~a{
                                   hello
                                   world

                                   })))

  (test-equal? "Multiline basic string with escaped newline"
               (parse-toml "x = \"\"\"hello\\\nworld\"\"\"")
               `#hasheq((x . "helloworld")))

  (test-equal? "Line ending backslash from TOML v1.0.0"
               (parse-result $ml-basic-string
                             @~a{"""
                                 The quick brown \


                                   fox jumps over \  
                                     the lazy dog."""})
               "The quick brown fox jumps over the lazy dog.")

  (test-equal? "Escaped newline with nothing after it"
               (parse-result $ml-basic-string "\"\"\"\\\r\n\"\"\"")
               "")

  (test-equal? "multiline basic string with tricky escape"
               (parse-toml @~a{multiline_end_esc = """When will it end? \"""...""\" should be here\""""})
               #hasheq((multiline_end_esc . "When will it end? \"\"\"...\"\"\" should be here\"")))

  (test-equal? "Multiline literal string with single quotes"
               (parse-result $ml-lit-string
                             @~a{''''quotes' and more quotes!'''''})
               "'quotes' and more quotes!''")

  (test-exn "Too many apostrophes!"
            exn:fail:parsack?
            (thunk (parse-toml "apos15 = '''Here are fifteen apostrophes: ''''''''''''''''''")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Array
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-equal? "Empty array"
               (parse-toml "x = [[[]]]")
               #hasheq((x . [[[]]])))

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

  (test-equal? "Parse array literal with comment after trailing comma"
               (parse-toml @~a{
                               array2 = [
                                         1, # test
                                         ]
                               })
               #hasheq((array2 . (1))))

  (test-equal? "Parse array literal with comment before trailing comma"
               (parse-toml @~a{
                               array2 = [
                                         1 # test
                                         ,
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
                              2 # comments allowed before commas
                              ,
                              3,
                              ] #comment
                   })
   #hasheq((array2 . (1 2 3))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Date
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-equal?
   "Parse date-time with various delimiters"
   (parse-toml @~a{
                   a = 1987-07-05 17:45:00Z
                   b = 1987-07-05t17:45:00z
                   c = 1987-07-05T17:45:00z
                   })
   (let ([t (moment 1987 7 5 17 45 #:tz 0)])
     `#hasheq((a . ,t)
              (b . ,t)
              (c . ,t))))

  (test-equal?
   "RFC 3339 with offset"
   (parse-toml @~a{odt2 = 1979-05-27T00:32:00-07:00
                   odt3 = 1979-05-27T00:32:00.999999-07:00})
   (hasheq 'odt2 (moment 1979 5 27 0 32 #:tz (* -7 3600))
           'odt3 (moment 1979 5 27 0 32 0 999999000 #:tz (* -7 3600))))

  (test-equal?
   "Local date-time"
   (parse-toml @~a{ldt1 = 1979-05-27T07:32:00
                   ldt2 = 1979-05-27T00:32:00.999999})
   (hasheq 'ldt1 (datetime 1979 5 27 7 32)
           'ldt2 (datetime 1979 5 27 0 32 0 999999000)))

  (test-equal? "Local date" ; FIXME
               (parse-result $datetime "1979-05-27")
               (date 1979 5 27))

  (test-equal? "Local time"
               (parse-toml @~a{lt1 = 07:32:00
                               lt2 = 00:32:00.999999})
               (hasheq 'lt1 (time 7 32)
                       'lt2 (time 0 32 0 999999000)))

  (test-equal?
   "Truncate, not round"
   (parse-toml @~a{odt = 1979-05-27T00:32:00.1234567899Z
                   ldt = 1979-05-27T00:32:00.9876543211
                   lt = 07:32:00.999999999999999})
   (hasheq 'odt (moment 1979 5 27 0 32 0 123456789 #:tz 0)
           'ldt (datetime 1979 5 27 0 32 0 987654321)
           'lt (time 7 32 0 999999999)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Inline Table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-equal? "Simple standard/inline table should be the same"
               (parse-toml "point = { x = 1, y = 2 }")
               (parse-toml "[point]\nx = 1\ny = 2")))

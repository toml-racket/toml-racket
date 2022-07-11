#lang at-exp racket/base


(require racket/function
         racket/format
         rackunit

         "../parsack.rkt"

         "../parsers/main.rkt")

(module+ test
  (test-not-exn "Parses emoji using $non-ascii"
                (thunk (parse-result $non-ascii "😂")))
  (test-exn "$non-ascii Refuses bad byte"
            #rx""
            (thunk (parse-result $non-ascii (open-input-bytes #"\xc3")))))

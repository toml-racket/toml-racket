#lang at-exp racket/base


(require racket/function
         racket/format
         rackunit

         "../parsack.rkt"

         "../parsers/main.rkt"
         "../parsers/shared.rkt")

(module+ test
  (test-not-exn "Parses emoji using $non-ascii"
                (thunk (parse-result $non-ascii "😂")))
  (test-exn "$non-ascii Refuses bad byte"
            #rx""
            (thunk (parse-result $non-ascii (open-input-bytes #"\xc3"))))

  (test-equal?
   "Parse with `repetition'"
   (parse-result (repetition (char #\a) 3) "aaaab")
   (list #\a #\a #\a)))

#lang at-exp racket/base


(require racket/function
         racket/format
         rackunit

         "../parsack.rkt"

         "../parsers/main.rkt")

(module+ test
  (test-not-exn "Parses a comment with UTF-8 characters in it"
                (thunk (parse-result $comment "# Hello world 😂\n")))

  (test-not-exn "Parses a comment with UTF-8 characters in it, but no newline"
                (thunk (parse-result $comment "# Hello world 😂")))

  (test-exn "Comment with control char is rejected"
            #rx".*comment.*"
            (thunk (parse-result $comment "# Hello world \0")))

  (test-exn "Comment with bad UTF-8 is rejected"
            #rx".*comment.*"
            (thunk (parse-result $comment (open-input-bytes #"# Ã")))))

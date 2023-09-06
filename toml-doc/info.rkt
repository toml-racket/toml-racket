#lang info

(define deps '("base"))
(define build-deps '("base"
                     "scribble-lib"
		     "racket-doc"
                     "toml-lib"))

(define scribblings '(("scribblings/toml.scrbl" () (parsing-library))))

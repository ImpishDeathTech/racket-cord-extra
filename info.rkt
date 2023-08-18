#lang info
(define collection "racket-cord")
(define deps '("base"
               "http-easy"
               "rfc6455"
               "rackunit-lib"
               "scribble-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/racket-cord.scrbl" ())))
(define pkg-desc "A modified version of the racket wrapper for the discord API for people who are lazy.")
(define version "0.1")
(define pkg-authors '(Ben Simms))

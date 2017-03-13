#lang info
(define collection "net")
(define deps '("fancy-app"
               "reprovide-lang"
               "compose-app"
               "functional-lib"
               "megaparsack-lib"
               ("base" #:version "6.5.0.900")))
(define build-deps '("net-doc"
                     "scribble-lib"
                     "scribble-text-lib"
                     "rackunit-lib"))
(define scribblings
  '(("scribblings/net-parse.scrbl" (multi-page) (parsing-library) "net-parse")))

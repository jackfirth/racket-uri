#lang info
(define collection "net")
(define deps '("fancy-app"
               "reprovide-lang"
               "compose-app"
               "functional-lib"
               "megaparsack-lib"
               ("base" #:version "6.5")))
(define build-deps '("scribble-lib"
                     "scribble-text-lib"
                     "rackunit-lib"))

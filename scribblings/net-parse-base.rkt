#lang racket/base

(provide (for-label (all-from-out racket/base
                                  racket/contract
                                  net/dns/parse))
         dns-examples)

(require (for-label racket/base
                    racket/contract
                    net/dns/parse)
         scribble/example)


(define (make-dns-eval) (make-base-eval '(require net/dns/parse)))

(define-syntax-rule (dns-examples exmpl ...)
  (examples #:eval (make-dns-eval) exmpl ...))

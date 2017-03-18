#lang racket/base

(provide (for-label (all-from-out racket/base
                                  racket/contract
                                  megaparsack
                                  megaparsack/text
                                  net/dns/parse
                                  net/ip/parse))
         dns-examples
         ip-examples)

(require (for-label racket/base
                    racket/contract
                    megaparsack
                    megaparsack/text
                    net/dns/parse
                    net/ip/parse)
         scribble/example
         syntax/parse/define)


(define-simple-macro (define-examples id:id req-clause ...+)
  (define-syntax-rule (id exmpl (... ...))
    (examples #:eval (make-base-eval '(require req-clause ...))
              exmpl (... ...))))

(define-examples dns-examples net/dns/parse megaparsack/text)
(define-examples ip-examples net/ip/parse megaparsack/text racket/function)

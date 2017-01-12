#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [ipv4-address/p (parser/c char? ipv4-address?)]))

(require compose-app/fancy-app
         data/applicative
         data/either
         megaparsack
         megaparsack/text
         net/ip/base)

(module+ test
  (require rackunit))


(define ip-part/p (guard/p integer/p (<= 0 _ 255) "integer in [0, 255]"))
(define ipv4-address-parts/p
  (many/p ip-part/p #:min 4 #:max 4 #:sep (char/p #\.)))
(define ipv4-address/p ((pure (apply ipv4-address _)) ipv4-address-parts/p))

(module+ test
  (check-equal? (parse-string ipv4-address/p "127.0.0.1") (success localhost))
  (check-equal? (parse-string ipv4-address/p "127.256.0.1")
                (failure (message (srcloc 'string 1 4 5 3)
                                  256
                                  '("integer in [0, 255]"))))
  (check-equal? (parse-string ipv4-address/p "127.0.0")
                (failure (message (srcloc #f #f #f #f #f)
                                  "end of input" '("'.'")))))

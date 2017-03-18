#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [ipv4-address (-> byte? byte? byte? byte? ipv4-address?)]
  [ipv4-address? predicate/c]
  [ipv4-address->bytes (-> ipv4-address? bytes?)]
  [ipv4-address->string (-> ipv4-address? string?)]
  [ipv4-address->list (-> ipv4-address? (list/c byte? byte? byte? byte?))]
  [localhost ipv4-address?]
  [localhost? predicate/c]
  [ipv4-address/p (parser/c char? ipv4-address?)]))

(require compose-app/fancy-app
         data/applicative
         data/either
         megaparsack
         megaparsack/text
         racket/function)

(module+ test
  (require rackunit))


(struct ipv4-address (first second third fourth)
  #:transparent #:omit-define-syntaxes)

(define localhost (ipv4-address 127 0 0 1))
(define localhost? (equal? _ localhost))

(module+ test
  (check-pred localhost? localhost))

(define (ipv4-address->list addr)
  (list (ipv4-address-first addr)
        (ipv4-address-second addr)
        (ipv4-address-third addr)
        (ipv4-address-fourth addr)))

(module+ test
  (check-equal? (ipv4-address->list localhost) (list 127 0 0 1)))

(define ipv4-address->bytes (list->bytes .. ipv4-address->list))

(module+ test
  (check-equal? (ipv4-address->bytes localhost)
                (bytes 127 0 0 1))
  (check-pred (negate immutable?) (ipv4-address->bytes localhost)))

(define ipv4-address->string
  (apply format "~a.~a.~a.~a" _ .. ipv4-address->list))

(module+ test
  (check-equal? (ipv4-address->string localhost) "127.0.0.1"))

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

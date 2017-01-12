#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [ipv4-address (-> byte? byte? byte? byte? ipv4-address?)]
  [ipv4-address? predicate/c]
  [ipv4-address->bytes (-> ipv4-address? bytes?)]
  [ipv4-address->dotted-string (-> ipv4-address? string?)]
  [ipv4-address->list (-> ipv4-address? (list/c byte? byte? byte? byte?))]
  [localhost ipv4-address?]))

(require compose-app/fancy-app
         racket/function)

(module+ test
  (require rackunit))


(struct ipv4-address (first second third fourth)
  #:transparent #:omit-define-syntaxes)

(define localhost (ipv4-address 127 0 0 1))

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

(define ipv4-address->dotted-string
  (apply format "~a.~a.~a.~a" _ .. ipv4-address->list))

(module+ test
  (check-equal? (ipv4-address->dotted-string localhost) "127.0.0.1"))

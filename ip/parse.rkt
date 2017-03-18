#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [ipv4-address (-> byte? byte? byte? byte? ipv4-address?)]
  [ipv4-address? predicate/c]
  [ipv4-address->bytes (->* (ipv4-address?) (#:immutable? boolean?) bytes?)]
  [ipv4-address->string (-> ipv4-address? string?)]
  [ipv4-address->list (-> ipv4-address? (list/c byte? byte? byte? byte?))]
  [ipv4-localhost ipv4-address?]
  [ipv4-localhost? predicate/c]
  [ipv4-address/p (parser/c char? ipv4-address?)]
  [string->ipv4-address
   (->* (string?) (#:failure-result failure-result/c) any/c)]))

(require compose-app/fancy-app
         data/applicative
         data/either
         megaparsack
         megaparsack/text
         net/dns/parse-string
         racket/function)

(module+ test
  (require doc-coverage
           rackunit))


(module+ test
  (check-all-documented 'net/ip/parse))

(struct ipv4-address (first second third fourth)
  #:transparent #:omit-define-syntaxes)

(define ipv4-localhost (ipv4-address 127 0 0 1))
(define ipv4-localhost? (equal? _ ipv4-localhost))

(module+ test
  (check-pred ipv4-localhost? ipv4-localhost))

(define (ipv4-address->list addr)
  (list (ipv4-address-first addr)
        (ipv4-address-second addr)
        (ipv4-address-third addr)
        (ipv4-address-fourth addr)))

(module+ test
  (check-equal? (ipv4-address->list ipv4-localhost) (list 127 0 0 1)))

(define (ipv4-address->bytes addr #:immutable? [immutable #t])
  (define mutable (list->bytes (ipv4-address->list addr)))
  (if immutable (bytes->immutable-bytes mutable) mutable))

(module+ test
  (check-equal? (ipv4-address->bytes ipv4-localhost)
                (bytes 127 0 0 1))
  (check-pred immutable? (ipv4-address->bytes ipv4-localhost))
  (check-pred (negate immutable?)
              (ipv4-address->bytes ipv4-localhost #:immutable? #f)))

(define ipv4-address->string
  (apply format "~a.~a.~a.~a" _ .. ipv4-address->list))

(module+ test
  (check-equal? (ipv4-address->string ipv4-localhost) "127.0.0.1"))

(define ip-part/p (guard/p integer/p (<= 0 _ 255) "integer in [0, 255]"))
(define ipv4-address-parts/p
  (many/p ip-part/p #:min 4 #:max 4 #:sep (char/p #\.)))
(define ipv4-address/p ((pure (apply ipv4-address _)) ipv4-address-parts/p))

(module+ test
  (check-equal? (parse-string ipv4-address/p "127.0.0.1")
                (success ipv4-localhost))
  (check-equal? (parse-string ipv4-address/p "127.256.0.1")
                (failure (message (srcloc 'string 1 4 5 3)
                                  256
                                  '("integer in [0, 255]"))))
  (check-equal? (parse-string ipv4-address/p "127.0.0")
                (failure (message (srcloc #f #f #f #f #f)
                                  "end of input" '("'.'")))))

(struct no-failure-result ())

(define (string->ipv4-address str #:failure-result [on-fail (no-failure-result)])
  (define (on-fail-handler err)
    (cond [(procedure? on-fail) (on-fail)]
          [(no-failure-result? on-fail) (raise-arguments-error/parse err str)]
          [else on-fail]))
  (either on-fail-handler values
          (parse-string ipv4-address/p str 'string->ipv4-address)))

(module+ test
  (check-equal? (string->ipv4-address "127.0.0.1") ipv4-localhost)
  (check-exn #rx"parse failure"
             (thunk (string->ipv4-address "foo")))
  (check-equal? (string->ipv4-address "foo" #:failure-result (thunk 'foo)) 'foo)
  (check-equal? (string->ipv4-address "foo" #:failure-result 'bar) 'bar))

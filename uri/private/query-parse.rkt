#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [key-value-query/p (parser/c char? key-value-query?)]))

(require compose-app
         data/applicative
         data/functor
         megaparsack
         megaparsack/text
         net/uri/base
         "applicative.rkt"
         "string-parse.rkt")

(module+ test
  (require data/either
           rackunit))


(define key-value-query-char/p
  (or/p uri-unreserved/p pct-decode/p uri-sub-delims/no-key-value/p))

(define key-value-query-pair/p
  ((pure cons) (map (string->symbol .. list->string)
                    (many+/p key-value-query-char/p))
               (seq (char/p #\=)
                    (map list->string (many/p key-value-query-char/p)))))

(module+ test
  (check-equal? (parse-string key-value-query-pair/p "foo=bar")
                (success (cons 'foo "bar"))))

(define key-value-query/p
  (map list->key-value-query
       (many/p key-value-query-pair/p #:sep (char/p #\&))))

(module+ test
  (check-equal? (parse-string key-value-query/p "foo=bar&baz=&blah=123")
                (success (key-value-query 'foo "bar" 'baz "" 'blah "123"))))

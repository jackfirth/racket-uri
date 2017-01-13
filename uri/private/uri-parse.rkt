#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [uri/p (->* (symbol?)
              (#:authority (parser/c char? authority?)
               #:path (parser/c char? uri-path?)
               #:query (parser/c char? query?))
              (parser/c char? uri?))]
  [postgresql-url/p (parser/c char? uri?)]
  [mysql-url/p (parser/c char? uri?)]
  [file-url/p (parser/c char? uri?)]))

(require compose-app/fancy-app
         data/applicative
         data/functor
         data/monad
         megaparsack
         megaparsack/text
         net/dns/base
         net/dns/parse
         net/uri/base
         "applicative.rkt"
         "authority-parse.rkt"
         "query-parse.rkt"
         "string-parse.rkt"
         "struct.rkt"
         "uri-path-parse.rkt")

(module+ test
  (require data/either
           rackunit))


(define (uri/p scheme
               #:authority [authority net-authority/p]
               #:path [path plain-uri-path/p]
               #:query [query key-value-query/p])
  (define uri/no-kws (uri _ #:authority _ #:path _ #:query _))
  ((pure uri/no-kws) (seq0 (symbol/p scheme) (char/p #\:))
                     (or/p (try/p (seq (string/p "//") authority)) (pure #f))
                     path
                     (or/p (seq (char/p #\?) query) (pure #f))))

(define postgresql-url/p (uri/p 'postgresql))
(define mysql-url/p (uri/p 'mysql))
(define file-url/p
  (uri/p 'file
         #:authority net-authority/empty/p
         #:query (pure #f)))

(module+ test
  (define test-postgresql-url-str
    "postgresql://jack:123@foo.com:789/my-db?some_param=123")
  (check-equal? (parse-string postgresql-url/p test-postgresql-url-str)
                (success
                 (uri 'postgresql
                      #:authority
                      (authority
                       (dns-reg-name (dns-address "foo" "com"))
                       #:userinfo (user+password-info "jack" #:password "123")
                       #:port 789)
                      #:path (plain-uri-path "my-db")
                      #:query (key-value-query 'some_param "123")))))

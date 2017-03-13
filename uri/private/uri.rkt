#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [uri (->* (symbol?)
            (#:authority (or/c authority? #f)
             #:path uri-path?
             #:query (or/c query? #f)
             #:fragment (or/c fragment? #f))
            uri?)]
  [uri? predicate/c]
  [uri-scheme (-> uri? symbol?)]
  [uri-authority (-> uri? (or/c authority? #f))]
  [uri-hier-path (-> uri? uri-path?)]
  [uri-query (-> uri? (or/c query? #f))]
  [uri-fragment (-> uri? (or/c fragment? #f))]))

(require "authority.rkt"
         "fragment.rkt"
         "query.rkt"
         "uri-path.rkt")

(module+ test
  (require rackunit))


(struct uri (scheme authority hier-path query fragment)
  #:transparent #:constructor-name make-uri #:omit-define-syntaxes)

(define (uri scheme
             #:authority [authority #f]
             #:path [path empty-uri-path]
             #:query [query #f]
             #:fragment [fragment #f])
  (make-uri scheme authority path query fragment))

(module+ test
  (check-equal? (uri 'foo)
                (uri 'foo
                     #:authority #f
                     #:path empty-uri-path
                     #:query #f
                     #:fragment #f)))

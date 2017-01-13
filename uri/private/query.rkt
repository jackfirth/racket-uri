#lang racket/base

(require racket/contract/base)

(provide
 query
 (contract-out
  [query? predicate/c]
  [plain-query (-> string? plain-query?)]
  [plain-query? predicate/c]
  [plain-query-value (-> plain-query? string?)]
  [key-value-query (unconstrained-domain-> key-value-query?)]
  [key-value-query? predicate/c]
  [key-value-query-params (-> key-value-query? key-value-query-hash?)]
  [list->key-value-query
   (-> (listof (cons/c symbol? string?)) key-value-query?)]
  [hash->key-value-query (-> key-value-query-hash? key-value-query?)]))

(require compose-app
         "struct.rkt")


(struct/supertype-only query)

(struct plain-query query (value) #:transparent)
(struct key-value-query query (params)
  #:transparent #:omit-define-syntaxes #:constructor-name hash->key-value-query)

(define (key-value-query . kvs)
  (hash->key-value-query (apply hash kvs)))

(define key-value-query-hash?
  (hash/c symbol? string? #:immutable #t #:flat? #t))

(define list->key-value-query (hash->key-value-query .. make-immutable-hash))

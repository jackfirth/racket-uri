#lang racket/base

(require racket/contract/base)

(provide
 path-segment
 (contract-out
  [path-segment? predicate/c]
  [plain-segment (-> string? plain-segment?)]
  [plain-segment? predicate/c]
  [plain-uri-path (->* () #:rest (listof string?) (uri-path/c plain-segment?))]
  [uri-path (->* () #:rest (listof path-segment?) uri-path?)]
  [uri-path? predicate/c]
  [uri-path/c (-> contract? contract?)]
  [uri-path-segments (-> uri-path? (listof path-segment?))]
  [same-level path-segment?]
  [up-one-level path-segment?]
  [empty-segment path-segment?]
  [empty-uri-path empty-uri-path?]
  [empty-uri-path? predicate/c]))

(require compose-app
         "struct.rkt")


(struct/supertype-only path-segment)
(struct/singleton-subtype same-level path-segment)
(struct/singleton-subtype up-one-level path-segment)
(struct/singleton-subtype empty-segment path-segment)

(struct uri-path (segments)
  #:transparent #:constructor-name make-uri-path #:omit-define-syntaxes)
(define (uri-path . segments) (make-uri-path segments))
(define (uri-path/c contract)
  (and/c uri-path? ((listof contract) .. uri-path-segments)))

(define empty-uri-path (uri-path))
(define (empty-uri-path? v) (equal? v empty-uri-path))

(struct plain-segment path-segment (str) #:transparent)
(define (plain-uri-path . strs) (apply uri-path (map plain-segment strs)))
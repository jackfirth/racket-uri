#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [seq (->* () #:rest (listof applicative?) applicative?)]
  [seq0 (->* () #:rest (listof applicative?) applicative?)]))

(require data/applicative
         data/functor
         racket/list)


(define (seq . applicatives)
  (map last (apply (pure list) applicatives)))

(define (seq0 . applicatives)
  (map first (apply (pure list) applicatives)))

#lang racket/base

(provide struct/supertype-only
         struct/singleton-subtype)

(require syntax/parse/define)

(define-simple-macro (struct/supertype-only id:id)
  (struct id () #:constructor-name private-constructor #:transparent))

(define-simple-macro (struct/singleton-subtype id:id parent:id)
  (begin
    (struct id parent () #:omit-define-syntaxes #:constructor-name make-instance)
    (define id (make-instance))))

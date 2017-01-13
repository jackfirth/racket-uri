#lang racket/base

(require racket/contract/base)

(provide
 fragment
 (contract-out
  [fragment? predicate/c]))

(require "struct.rkt")


(struct/supertype-only fragment)

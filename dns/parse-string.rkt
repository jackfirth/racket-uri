#lang racket/base

(provide char-alphabetic-ascii?
         char-numeric-ascii?
         stringof
         string-last
         string-trim-ends)

(require compose-app/fancy-app
         racket/function)

(module+ test
  (require rackunit))


(define char-alphabetic-ascii?
  (disjoin (char<=? #\a _ #\z) (char<=? #\A _ #\Z)))

(define char-numeric-ascii? (char<=? #\0 _ #\9))
(define (stringof char-pred) (andmap char-pred _ .. string->list))
(define (string-last str) (string-ref str (sub1 (string-length str))))

(module+ test
  (check-true ((stringof char-alphabetic-ascii?) "abcd"))
  (check-true ((stringof char-alphabetic-ascii?) "ABCD"))
  (check-true ((stringof char-alphabetic-ascii?) "abCD"))
  (check-false ((stringof char-alphabetic-ascii?) "ab123"))
  (check-false ((stringof char-alphabetic-ascii?) "abCDλ"))
  (check-true ((stringof char-numeric-ascii?) "123")))

(define (string-trim-ends str)
  (define len (string-length str))
  (if (< len 3) "" (substring str 1 (sub1 len))))

(module+ test
  (check-equal? (string-trim-ends "") "")
  (check-equal? (string-trim-ends "ab") "")
  (check-equal? (string-trim-ends "abba") "bb"))

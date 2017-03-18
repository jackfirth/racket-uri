#lang racket/base

(provide char-alphabetic-ascii?
         char-numeric-ascii?
         raise-arguments-error/parse
         stringof
         string-last
         string-trim-ends)

(require compose-app/fancy-app
         megaparsack
         racket/format
         racket/function
         racket/list
         racket/match
         racket/string)

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

(define (list/filter . vs) (filter values vs))
(define error-detail (format "  ~a: ~a" _ _))

(define (raise-arguments-error/parse parse-err str)
  (match-define (message srcloc unexpected expected) parse-err)
  (define source-message
    (string-join (map ~a
                      (list/filter (srcloc-source srcloc)
                                   (srcloc-line srcloc)
                                   (srcloc-column srcloc)))
                 ":"))
  (define leading-message-parts
    (list/filter (and (non-empty-string? source-message) source-message)
                 "parse failure"))
  (define leading-message (string-join leading-message-parts ": "))
  (define str-column (srcloc-column srcloc))
  (define str-line (srcloc-line srcloc))
  (define expected/sort (sort (remove-duplicates expected) string<=?))
  (define expected-part
    (cond [(empty? expected/sort) #f]
          [(< (length expected/sort) 3) (string-join expected/sort " or ")]
          [else (string-join expected/sort ", " #:before-last ", or ")]))
  (define msg-parts
    (list/filter leading-message
                 (error-detail 'string (~v str))
                 (error-detail 'unexpected unexpected)
                 (and expected-part (error-detail 'expected expected-part))))
  (define msg (string-join msg-parts "\n"))
  (raise (make-exn:fail:contract msg (current-continuation-marks))))

(module+ test
  (require data/either
           megaparsack/text)
  (define ((parse-error-thunk parser str))
    (raise-arguments-error/parse (from-either (parse-string parser str 'test))
                                 str))
  (define integer-foo-thunk (parse-error-thunk integer/p "foo"))
  (check-exn exn:fail:contract? integer-foo-thunk)
  (check-exn #rx"test:1:0: parse failure" integer-foo-thunk)
  (check-exn #rx"  string: \"foo\"" integer-foo-thunk)
  (check-exn #rx"  unexpected: f" integer-foo-thunk)
  (check-exn #rx"  expected: integer" integer-foo-thunk)
  (define ab-thunk
    (parse-error-thunk (or/p (char/p #\a) (char/p #\b)) "foo"))
  (check-exn #rx"'a' or 'b'" ab-thunk)
  (define abc-thunk
    (parse-error-thunk (or/p (char/p #\a) (char/p #\b) (char/p #\c)) "foo"))
  (check-exn #rx"'a', 'b', or 'c'" abc-thunk)
  (define (exn/message? pattern)
    (conjoin exn? (regexp-match? pattern _ .. exn-message)))
  (check-exn (conjoin (exn/message? #rx"parse failure")
                      (exn/message? #rx" unexpected")
                      (negate (exn/message? #rx" expected")))
             (parse-error-thunk (fail/p (message (srcloc 'blah 1 0 1 0) "whatever" (list)))
                                "foo")))

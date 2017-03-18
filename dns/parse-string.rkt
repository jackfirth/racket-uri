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

(define (raise-arguments-error/parse parse-err
                                     str
                                     #:source [source #f]
                                     #:include-column? [column? #t]
                                     #:include-line? [line? #t])
  (match-define (message srcloc unexpected expected) parse-err)
  (define expected/sort (sort (remove-duplicates expected) string<=?))
  (define expected-part
    (cond [(empty? expected/sort) #f]
          [(< (length expected/sort) 3) (string-join expected/sort " or ")]
          [else (string-join expected/sort ", " #:before-last ", or ")]))
  (define leading-message-parts
    (list/filter (or (~a source) (srcloc-source srcloc)) "parse failure"))
  (define leading-message (string-join leading-message-parts ": "))
  (define str-column (srcloc-column srcloc))
  (define str-line (srcloc-line srcloc))
  (define msg-parts
    (list/filter leading-message
                 (error-detail 'string (~v str))
                 (error-detail 'unexpected unexpected)
                 (error-detail 'expected expected-part)
                 (and line? str-line (error-detail 'line str-line))
                 (and column? str-column (error-detail 'column str-column))))
  (define msg (string-join msg-parts "\n"))
  (raise (make-exn:fail:contract msg (current-continuation-marks))))

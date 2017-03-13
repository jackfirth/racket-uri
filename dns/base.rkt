#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [dns-address (->* () #:rest (listof dns-subdomain?) dns-address?)]
  [dns-address? predicate/c]
  [dns-address->list (-> dns-address? (listof dns-subdomain?))]
  [dns-address->string (-> dns-address? string?)]
  [dns-localhost dns-address?]
  [dns-localhost? predicate/c]
  [dns-root dns-root?]
  [dns-root? predicate/c]
  [dns-subdomain? predicate/c]))

(require compose-app/fancy-app
         racket/function
         racket/list
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

(define dns-subdomain-char?
  (disjoin char-alphabetic-ascii? char-numeric-ascii? (equal? _ #\-)))

(module+ test
  (check-true (dns-subdomain-char? #\a))
  (check-true (dns-subdomain-char? #\A))
  (check-true (dns-subdomain-char? #\0))
  (check-true (dns-subdomain-char? #\-))
  (check-false (dns-subdomain-char? #\λ))
  (check-false (dns-subdomain-char? #\+)))

(define dns-subdomain?
  (conjoin (< 0 _ 64 .. string-length)
           (char-alphabetic-ascii? .. string-ref _ 0)
           (stringof dns-subdomain-char? .. string-trim-ends)
           (disjoin char-alphabetic-ascii? char-numeric-ascii? .. string-last)))

(module+ test
  (check-false (dns-subdomain? ""))
  (check-true (dns-subdomain? "a"))
  (check-false (dns-subdomain? "0"))
  (check-false (dns-subdomain? "-"))
  (check-false (dns-subdomain? "λ"))
  (check-true (dns-subdomain? "aA"))
  (check-true (dns-subdomain? "A0"))
  (check-false (dns-subdomain? "a-"))
  (check-true (dns-subdomain? "a-0"))
  (check-true
   (dns-subdomain?
    "aaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccddd"))
  (check-false
   (dns-subdomain?
    "aaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccdddd")))

(struct dns-address (parts)
  #:transparent #:omit-define-syntaxes #:constructor-name make-dns-address)

(define (dns-address . parts) (make-dns-address parts))
(define dns-root (dns-address))
(define dns-root? (equal? _ dns-root))

(define dns-address->list dns-address-parts)
(define (dns-address->string addr)
  (define parts (dns-address->list addr))
  (if (empty? parts) "." (string-join parts ".")))

(module+ test
  (check-equal? (dns-address->string dns-root) ".")
  (check-equal? (dns-address->string (dns-address "www" "google" "com"))
                "www.google.com"))

(define dns-localhost (dns-address "localhost"))
(define dns-localhost? (equal? _ dns-localhost))

(module+ test
  (check-pred dns-localhost? dns-localhost))

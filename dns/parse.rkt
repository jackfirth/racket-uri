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
  [dns-subdomain? (-> string? boolean?)]
  [dns-subdomain/p (parser/c char? dns-subdomain?)]
  [dns-address/p (parser/c char? dns-address?)]
  [port-number/p (parser/c char? port-number?)]))

(require compose-app/fancy-app
         data/applicative
         data/either
         data/monad
         megaparsack
         megaparsack/text
         racket/function
         racket/list
         racket/string
         racket/tcp
         "parse-string.rkt")

(module+ test
  (require rackunit))


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

(define char-letter-ascii/p
  (label/p "an ascii character"
           (or/p (char-between/p #\a #\z) (char-between/p #\A #\Z))))

(define char-numeric-ascii/p
  (label/p "an ascii numeric character" (char-between/p #\0 #\9)))

(define dns-subdomain-char/p
  (or/p char-letter-ascii/p char-numeric-ascii/p (char/p #\-)))

(define dns-trailing-subdomain-char/p
  (or/p char-letter-ascii/p char-numeric-ascii/p))

(define dns-subdomain-chars/p
  (guard/p (or/p ((pure append) (list/p char-letter-ascii/p)
                                (many/p dns-subdomain-char/p #:max 63))
                 (list/p char-letter-ascii/p))
           (negate (equal? _ #\-) .. last)
           "dns subdomain without trailing slash"))

(define dns-subdomain/p
  (guard/p ((pure list->string) dns-subdomain-chars/p)
           (< _ 64 .. string-length)
           "dns subdomain less than 64 chars long"))

(module+ test
  (define 63-char-subdomain
    "aaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccddd")
  (define 64-char-subdomain
    "aaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccddddaaaabbbbccccdddd")
  (check-equal? (parse-string dns-subdomain/p "google") (success "google"))
  (check-equal? (parse-string dns-subdomain/p 63-char-subdomain)
                (success 63-char-subdomain))
  (check-equal? (parse-string dns-subdomain/p 64-char-subdomain)
                (failure (message (srcloc 'string 1 0 1 64)
                                  64-char-subdomain
                                  '("dns subdomain less than 64 chars long")))))

(define dns-address-parts-short-enough?
  (< _ 256 .. string-length .. apply string-append _ .. add-between _ "."))

(module+ test
  (check-true (dns-address-parts-short-enough? '("www" "google" "com"))))

(define too-long-message
  "dns address with total length less than 256 (including dots)")

(define dns-address-subdomains/p
  (guard/p (many/p dns-subdomain/p #:sep (char/p #\.) #:min 1)
           dns-address-parts-short-enough?
           too-long-message
           (string-join _ ".")))

(define root/p (do (char/p #\.) (pure dns-root)))

(define dns-address/p
  (or/p ((pure (apply dns-address _)) dns-address-subdomains/p) root/p))

(module+ test
  (define 255-char-address
    (string-append 63-char-subdomain "."
                   63-char-subdomain "."
                   63-char-subdomain "."
                   63-char-subdomain))
  (define 257-char-address
    (string-append 63-char-subdomain "."
                   63-char-subdomain "."
                   63-char-subdomain "."
                   63-char-subdomain ".a"))
  (check-equal? (parse-string dns-address/p ".") (success dns-root))
  (check-equal? (parse-string dns-address/p "www.google.com")
                (success (dns-address "www" "google" "com")))
  (check-equal? (parse-string dns-address/p 255-char-address)
                (success
                 (dns-address 63-char-subdomain
                              63-char-subdomain
                              63-char-subdomain
                              63-char-subdomain)))
  (check-equal? (parse-string dns-address/p 257-char-address)
                (failure
                 (message (srcloc 'string 1 0 1 257)
                          257-char-address
                          (list too-long-message)))))

(define port-description "port number between 1 and 65535")
(define port-number/p (guard/p integer/p port-number? port-description))

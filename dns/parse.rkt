#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [dns-address (->* () #:rest (listof dns-subdomain?) dns-address?)]
  [dns-address? predicate/c]
  [dns-address->list (-> dns-address? (listof dns-subdomain?))]
  [dns-address->string (->* (dns-address?)
                            (#:trailing-dot? boolean?
                             #:punycode? boolean?)
                            string?)]
  [dns-localhost dns-address?]
  [dns-localhost? predicate/c]
  [dns-root dns-root?]
  [dns-root? predicate/c]
  [dns-subdomain? (-> string? boolean?)]
  [dns-subdomain/p (parser/c char? dns-subdomain?)]
  [dns-address/p (parser/c char? dns-address?)]
  [port-number/p (parser/c char? port-number?)]
  [string->dns-address
   (->* (string?) (#:failure-result failure-result/c) any/c)]))

(require compose-app/fancy-app
         data/applicative
         data/either
         (rename-in data/functor [map fmap])
         data/monad
         megaparsack
         megaparsack/text
         racket/function
         racket/list
         racket/string
         racket/tcp
         "parse-punycode.rkt"
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

(define dns-address-parts-short-enough?
  (< _ 256 .. string-length .. string-join _ "."))

(module+ test
  (check-pred dns-address-parts-short-enough? (list "www" "google" "com"))
  (check-pred dns-address-parts-short-enough? (list (make-string 255 #\a)))
  (check-pred dns-address-parts-short-enough? (list (make-string 253 #\a) "b"))
  (check-pred (negate dns-address-parts-short-enough?)
              (list (make-string 256 #\a)))
  (check-pred (negate dns-address-parts-short-enough?)
              (list (make-string 254 #\a) "b")))

(struct dns-address (parts)
  #:transparent #:omit-define-syntaxes #:constructor-name make-dns-address)

(define (dns-address . parts)
  (unless (dns-address-parts-short-enough? parts)
    (raise-arguments-error 'dns-address
                           "total length of dot-joined subdomains exceeds 255"
                           "subdomains" parts))
  (make-dns-address (map string-downcase parts)))

(module+ test
  (check-not-exn (thunk (dns-address (make-string 255 #\a))))
  (check-exn exn:fail:contract? (thunk (dns-address (make-string 256 #\a))))
  (check-equal? (dns-address "www" "google" "com")
                (dns-address "www" "GOOGLE" "com"))
  (check-not-equal? (dns-address "www" "google" "com")
                    (dns-address "www" "foogle" "com")))

(define dns-root (dns-address))
(define dns-root? (equal? _ dns-root))
(define dns-address->list dns-address-parts)

(define (dns-address->string addr
                             #:trailing-dot? [dot? #f]
                             #:punycode? [punycode? #t])
  (define parts/raw (dns-address->list addr))
  (define parts (if punycode? parts/raw (map punycode-encode parts/raw)))
  (define joined-parts (string-join parts "."))
  (cond [(empty? parts) "."]
        [dot? (string-append joined-parts ".")]
        [else joined-parts]))

(module+ test
  (check-equal? (dns-address->string dns-root) ".")
  (check-equal? (dns-address->string (dns-address "www" "google" "com"))
                "www.google.com")
  (check-equal? (dns-address->string (dns-address "www" "google" "com")
                                     #:trailing-dot? #t)
                "www.google.com.")
  ;; these tests are incorrect, but punycode isn't yet implemented
  (check-equal? (dns-address->string (dns-address "www" "göögle" "com"))
                "www.göögle.com")
  (check-equal? (dns-address->string (dns-address "www" "göögle" "com")
                                     #:punycode? #f)
                "www.göögle.com"))

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
  (guard/p (or/p ((pure cons) char-letter-ascii/p
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

(define too-long-message
  "dns address with total length less than 256 (including dots)")

(define dns-address-subdomains/p
  (guard/p (many/p dns-subdomain/p #:sep (char/p #\.) #:min 1)
           dns-address-parts-short-enough?
           too-long-message
           (string-join _ ".")))

(define root/p (do (char/p #\.) (pure dns-root)))

(define dns-address/p  
  (or/p (fmap (apply dns-address _) dns-address-subdomains/p) root/p))

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

(struct no-failure-result ())

(define (string->dns-address str #:failure-result [on-fail (no-failure-result)])
  (define (raise-parse-contract-exn err)
    (raise-arguments-error/parse err str
                                 #:source 'string->dns-address
                                 #:include-column? #f
                                 #:include-line? #f))
  (define (on-fail-handler err)
    (cond [(procedure? on-fail) (on-fail)]
          [(no-failure-result? on-fail) (raise-parse-contract-exn err)]
          [else on-fail]))
  (define str/no-dot
    (if (and (string-suffix? str ".")
             (not (equal? (string-length str) 1)))
        (substring str 0 (sub1 (string-length str)))
        str))
  (either on-fail-handler values (parse-string dns-address/p str/no-dot)))

(module+ test
  (check-equal? (string->dns-address "www.google.com")
                (dns-address "www" "google" "com"))
  (check-exn exn:fail:contract? (thunk (string->dns-address "21847")))
  (check-exn #rx"64" (thunk (string->dns-address (make-string 65 #\a))))
  (check-equal? (string->dns-address "www.google.com.")
                (string->dns-address "www.google.com"))
  (check-equal? (string->dns-address ".") dns-root)
  (check-equal? (string->dns-address "123" #:failure-result 'foo) 'foo)
  (check-equal? (string->dns-address "123" #:failure-result (thunk 'foo)) 'foo))

(define port-description "port number between 1 and 65535")
(define port-number/p (guard/p integer/p port-number? port-description))

#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [authority/p (->* ()
                    (#:userinfo (parser/c char? userinfo?)
                     #:reg-name (parser/c char? reg-name?))
                    (parser/c char? authority?))]
  [net-authority/p (parser/c char? net-authority?)]
  [password-str/p (parser/c char? string?)]
  [username-str/p (parser/c char? string?)]))

(require data/applicative
         data/functor
         fancy-app
         megaparsack
         megaparsack/text
         net/dns/base
         net/dns/parse
         net/ip/parse
         net/uri/base
         racket/tcp
         "applicative.rkt"
         "string-parse.rkt")

(module+ test
  (require data/either
           rackunit))


(define host/p (or/p ipv4-address/p _))

(define username-char/p (or/p uri-unreserved/p pct-decode/p uri-sub-delims/p))
(define username-str/p (stringof username-char/p))
(define userinfo-char/p (or/p username-char/p (char/p #\:)))
(define userinfo-str/p (stringof userinfo-char/p))
(define password-str/p (or/p (seq (char/p #\:) userinfo-str/p) (pure #f)))

(define user+password-info/p
  ((pure (user+password-info _ #:password _)) username-str/p password-str/p))

(module+ test
  (check-equal? (parse-string user+password-info/p
                              "j%61ck:secret-password::!!isthebest")
                (success
                 (user+password-info
                  "jack" #:password "secret-password::!!isthebest"))))

(define (authority/p #:userinfo [userinfo user+password-info/p]
                     #:reg-name [reg-name (map dns-reg-name dns-address/p)])
  (define userinfo-opt (or/p (try/p (seq0 userinfo (char/p #\@))) (pure #f)))
  (define port-opt (or/p (seq (char/p #\:) port-number/p) (pure #f)))
  (define (authority/parsed-args userinfo reg-name port)
    (authority reg-name #:userinfo userinfo #:port port))
  ((pure authority/parsed-args) userinfo-opt (host/p reg-name) port-opt))

(define net-authority/p (authority/p))

(module+ test
  (define google-reg-name (dns-reg-name (dns-address "google" "com")))
  (check-equal? (parse-string net-authority/p "jack:password@google.com:443")
                (success (authority google-reg-name
                                    #:userinfo (user+password-info
                                                "jack" #:password "password")
                                    #:port 443)))
  (check-equal? (parse-string net-authority/p "google.com")
                (success (authority google-reg-name)))
  (check-equal? (parse-string net-authority/p "jack@google.com")
                (success (authority google-reg-name
                                    #:userinfo (user+password-info "jack")))))

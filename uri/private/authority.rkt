#lang racket/base

(require racket/contract/base)

(provide
 reg-name
 userinfo
 (contract-out
  [authority (->* (host?)
                  (#:userinfo (or/c userinfo? #f) #:port (or/c port-number? #f))
                  authority?)]
  [authority? predicate/c]
  [authority/c (->* () (#:host predicate/c #:userinfo predicate/c)
                    predicate/c)]
  [authority-host (-> authority? host?)]
  [authority-userinfo (-> authority? (or/c userinfo? #f))]
  [authority-port (-> authority? (or/c port-number? #f))]
  [dns-reg-name (-> dns-address? dns-reg-name?)]
  [dns-reg-name? predicate/c]
  [dns-reg-name-address (-> dns-reg-name? dns-address?)]
  [empty-reg-name empty-reg-name?]
  [empty-reg-name? predicate/c]
  [host? predicate/c]
  [net-host? predicate/c]
  [net-authority? predicate/c]
  [net-authority/empty? predicate/c]
  [reg-name? predicate/c]
  [userinfo? predicate/c]
  [user+password-info (->* (string?) (#:password (or/c string? #f))
                           user+password-info?)]
  [user+password-info? predicate/c]
  [user+password-info-user (-> user+password-info? string?)]
  [user+password-info-password (-> user+password-info? (or/c string? #f))]))

(require compose-app/fancy-app
         net/dns/base
         net/ip/base
         racket/function
         racket/tcp
         "struct.rkt")

(struct/supertype-only userinfo)

(struct user+password-info userinfo (user password)
  #:transparent #:omit-define-syntaxes #:constructor-name make-user+password-info)

(define (user+password-info user #:password [password #f])
  (make-user+password-info user password))

(struct/supertype-only reg-name)
(struct/singleton-subtype empty-reg-name reg-name)
(struct dns-reg-name reg-name (address) #:transparent #:omit-define-syntaxes)

(define host? (or/c ipv4-address? reg-name?))
(define net-host? (or/c ipv4-address? dns-reg-name?))

(struct authority (host userinfo port)
  #:transparent #:constructor-name make-authority #:omit-define-syntaxes)

(define (authority host #:userinfo [userinfo #f] #:port [port #f])
  (make-authority host userinfo port))

(define (authority/c #:host [host-pred host?]
                     #:userinfo [userinfo-pred userinfo?]
                     #:port [port-pred (const #t)])
  (and/c authority?
         (host-pred .. authority-host)
         (userinfo-pred .. authority-userinfo)
         (port-pred .. authority-port)))

(define net-authority?
  (authority/c #:host net-host? #:userinfo (or/c user+password-info? #f)))

(define net-authority/empty?
  (authority/c #:host (or/c empty-reg-name?
                            localhost?
                            (dns-localhost? .. dns-reg-name-address))
               #:userinfo #f
               #:port not))

#lang scribble/manual

@(require "net-parse-base.rkt")

@title{Parsing DNS Addresses}
@defmodule[net/dns/parse]

@defproc[(dns-address [subdomain (and/c string? dns-subdomain?)] ...)
         dns-address?]{
 Constructs a DNS address composed of the given @racket[subdomain]s. Each
 subdomain is converted to lower case with @racket[string-downcase], as DNS
 addresses are formally case insensitive.
 @(dns-examples
   (dns-address "www" "google" "com")
   (dns-address "www" "Google" "com"))}

@defproc[(dns-address? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is a DNS address as constructed with
 @racket[dns-address].
 @(dns-examples
   (dns-address? (dns-address "www" "google" "com"))
   (dns-address? 5)
   (dns-address? "www.google.com"))}

@defproc[(dns-subdomain? [str string?]) boolean?]{
 Returns @racket[#t] when @racket[str] is a DNS subdomain. Subdomains are
 nonempty strings with fewer than 64 characters. The first character must be an
 alphabetic ASCII character. Subsequent characters must be ASCII alphabetic or
 numeric characters or the hyphen character. The subdomain must not end with a
 hyphen. DNS addresses are formally case-insensitive, although the
 @racket[dns-address] constructor allows preserving case information.
 @(dns-examples
   (dns-subdomain? "google")
   (dns-subdomain? "göögle")
   (dns-subdomain? "8oogle")
   (dns-subdomain? "google-"))}

@defproc[(dns-address->string [addr dns-address?]
                              [#:trailing-dot? dot? boolean? #f]
                              [#:punycode? punycode? boolean? #t])
         string?]{
 Returns the string form of @racket[addr], which consists of joining the
 subcomponents of @racket[addr] with dots. If @racket[dot] is @racket[#t], a
 trailing dot is added to represent the root domain name. If @racket[punycode?]
 is @racket[#t], Unicode characters in the domain name are encoded in ASCII
 using the Punycode standard. Otherwise Unicode characters are included verbatim
 in the output string, making it suitable for display purposes only.
 @(dns-examples
   (dns-address->string (dns-address "www" "google" "com"))
   (dns-address->string (dns-address "www" "google" "com") #:trailing-dot? #t))}

@defproc[(string->dns-address [str string?]
                              [#:failure-result on-fail failure-result/c
                               (lambda ()
                                 (raise (make-exn:fail:contract ....)))])
         (or/c dns-address? any/c)]{
 Parses a DNS address from @racket[str]. Each dot-separated portion of
 @racket[str] must be a legal DNS subdomain. Specifically, each substring in
 @racket[(string-split str #\.)] must be a subdomain in the sense of
 @racket[dns-subdomain?]. Additionally, the total length of the dns address's
 Punycode string representation including dots (but not with a trailing dot)
 must be 255 characters or less. This is a separate length restriction from the
 per-subdomain restriction of 63 characters.

 If parsing fails due to malformed input or a violation of the above conditions,
 @racket[on-fail] is called to determine how to proceed. By default, a contract
 exception is thrown identifying the cause of failure. To achieve behavior
 similar to @racket[string->number] where @racket[#f] is returned, use
 @racket[#:failure-result #f].

 @(dns-examples
   (string->dns-address "www.google.com")
   (string->dns-address "www.google.com.")
   (eval:error (string->dns-address "123"))
   (string->dns-address "123" #:failure-result #f))}

@defproc[(dns-address->list [addr dns-address?])
         (listof (and/c string? dns-subdomain?))]{
 Returns a list of the subdomains in @racket[addr].
 @(dns-examples
   (dns-address->list (dns-address "www" "google" "com")))}

@deftogether[(@defthing[dns-root dns-address?]
               @defproc[(dns-root? [v any/c]) boolean?])]{
 The root dns address, and a predicate that recognizes the root address. All DNS
 addresses implicitly refer to this address at the top of the domain name
 hierarchy. Equivalent to @racket[(dns-address)].}

@deftogether[(@defthing[dns-localhost dns-address?]
               @defproc[(dns-localhost? [v any/c]) boolean?])]{
 The localhost DNS address (referring to whatever the local machine is), and a
 predicate that recognizes the localhost address. Equivalent to
 @racket[(dns-address "localhost")].}

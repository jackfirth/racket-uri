#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     net/dns/parse))

@title{Parsing DNS Addresses}
@defmodule[net/dns/parse]

@defproc[(dns-address [subdomain (and/c string? dns-subdomain?)] ...
                      [#:normalize-case? normalize? boolean? #t])
         dns-address?]{
 Constructs a DNS address composed of the given @racket[subdomain]s. If
 @racket[normalize?] is true, each subdomain is converted to lower case with
 @racket[string-downcase]. DNS addresses can be normalized after construction
 with @racket[dns-address-normalize].}

@defproc[(dns-address? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is a DNS address as constructed with
 @racket[dns-address].}

@defproc[(dns-subdomain? [str string?]) boolean?]{
 Returns @racket[#t] when @racket[str] is a DNS subdomain. Subdomains are
 nonempty strings with fewer than 64 characters. The first character must be an
 alphabetic ASCII character. Subsequent characters must be ASCII alphabetic or
 numeric characters or the hyphen character. The subdomain must not end with a
 hyphen. DNS addresses are formally case-insensitive, although the
 @racket[dns-address] constructor allows preserving case information.}

@defproc[(dns-address-normalize [addr dns-address?]) dns-address?]{
 Returns an address equivalent to @racket[addr] but in normalized form. Each
 subdomain is converted to lowercase form with @racket[string-downcase].}

@defproc[(dns-address->string [addr dns-address?]
                              [#:trailing-dot? dot? boolean? #f]
                              [#:unicode? unicode? boolean? #f])
         string?]{
 Returns the string form of @racket[addr], which consists of joining the
 subcomponents of @racket[addr] with dots. If @racket[dot] is @racket[#t], a
 trailing dot is added to represent the root domain name. If @racket[unicode?]
 is @racket[#t], the output string may contain Unicode characters making it an
 illegal string representation of a DNS address. Such a string should only be
 used for display purposes. If @racket[unicode?] is @racket[#f], Unicode
 characters in domain name are encoded in ASCII using the Punycode standard.}

@defproc[(string->dns-address [str string?]
                              [#:normalize-case? normalize? boolean? #t]
                              [#:trailing-dot? dot? boolean? #t]
                              [#:decode-punycode? punycode? boolean? #t]
                              [#:allow-unicode? unicode? boolean? #f]
                              [#:failure-result on-fail failure-result/c
                               (lambda ()
                                 (raise (make-exn:fail:contract ....)))])
         (or/c dns-address? any/c)]{
 Parses a DNS address from @racket[str]. If @racket[dot?] is @racket[#t], then
 DNS addresses with a trailing dot are accepted and parse the same as if the dot
 was omitted. If @racket[punycode] is @racket[#t], then any Punycode-encoded
 Unicode characters will be included from @racket[str]. Otherwise, Punycode
 encodings are interpreted literally. If @racket[unicode?] is true, Unicode
 characters may be directly included in @racket[str] and can be converted later
 to Punycode with @racket[dns-address->string]. The @racket[normalize?] argument
 has the same meaning as it does in @racket[dns-address]: when @racket[#t], the
 resulting address is converted into lowercase.

 Each dot-separated portion of @racket[str] must be a legal DNS subdomain.
 Specifically, each substring in @racket[(string-split str #\.)] must be a
 subdomain in the sense of @racket[dns-subdomain?]. Additionally, the total
 length of the dns address's Punycode string representation including dots (but
 not with a trailing dot) must be 255 characters or less. This is a separate
 length restriction from the per-subdomain restriction of 63 characters.

 If parsing fails due to malformed input or a violation of the above conditions,
 @racket[on-fail] is called to determine how to proceed. By default, a contract
 exception is thrown identifying the cause of failure. To achieve behavior
 similar to @racket[string->number] where @racket[#f] is returned, use
 @racket[#:failure-result #f].}

@defproc[(dns-address->list [addr dns-address?])
         (listof (and/c string? dns-subdomain?))]{
 Returns a list of the subdomains in @racket[addr].}

@defthing[dns-root dns-address?]{
 The root dns address. All DNS addresses implicitly refer to this address at the
 top of the domain name hierarchy. Equivalent to @racket[(dns-address)].}

@defproc[(dns-root? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is equal to @racket[dns-root].}

@defthing[dns-localhost dns-address?]{
 The localhost DNS address, referring to whatever the local machine is.
 Equivalent to @racket[(dns-address "localhost")].}

@defproc[(dns-localhost? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is equal to @racket[dns-localhost].}

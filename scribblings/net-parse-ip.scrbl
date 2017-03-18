#lang scribble/manual

@(require "net-parse-base.rkt")

@title{Parsing IP Addresses}
@defmodule[net/ip/parse]

@defproc[(ipv4-address [part1 byte?] [part2 byte?] [part3 byte?] [part4 byt?])
         ipv4-address?]{
 Constructs an IPv4 address from the given bytes.
 @(ip-examples
   (ipv4-address 127 0 0 1))}

@defproc[(ipv4-address? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is an IPv4 address constructed with
 @racket[ipv4-address], and @racket[#f] otherwise.
 @(ip-examples
   (ipv4-address? ipv4-localhost)
   (ipv4-address? 'foo))}

@defproc[(ipv4-address->list [addr ipv4-address?])
         (list byte? byte? byte? byte?)]{
 Returns a list of the byte values of @racket[addr].
 @(ip-examples
   (ipv4-address->list ipv4-localhost))}

@defproc[(ipv4-address->bytes [addr ipv4-address?] [#:immutable? immutable? #t])
         bytes?]{
 Returns a bytestring comprised of the four bytes of @racket[addr]. The returned
 bytestring is immutable when @racket[immutable?] is @racket[#t], and mutable
 otherwise.
 @(ip-examples
   (ipv4-address->bytes ipv4-localhost))}

@defproc[(ipv4-address->string [addr ipv4-address?]) string?]{
 Returns the string form of @racket[addr].
 @(ip-examples
   (ipv4-address->string ipv4-localhost))}

@defproc[(string->ipv4-address [str string?]
                               [#:failure-result on-fail failure-result/c
                                (lambda ()
                                  (raise (make-exn:fail:contract ....)))])
         any/c]{
 Parses @racket[str] into an @racket[ipv4-address]. In the event of a parse
 failure @racket[on-fail] is called if it is a procedure and its result is
 returned, otherwise @racket[on-fail] is returned directly. Parse failures
 include malformed input and inputs that are out-of-bounds for IPv4 addresses.
 @(ip-examples
   (string->ipv4-address "127.0.0.1")
   (eval:error (string->ipv4-address "foo"))
   (string->ipv4-address "foo" #:failure-result (thunk (display "uh oh!")))
   (string->ipv4-address "foo" #:failure-result #f))}

@deftogether[
 (@defthing[ipv4-localhost ipv4-address?]
   @defproc[(ipv4-localhost? [v any/c]) boolean?])]{
 The localhost IPv4 address and a predicate that recognizes that address.
 @(ip-examples
   ipv4-localhost
   (ipv4-localhost? ipv4-localhost)
   (ipv4-localhost? (ipv4-address 192 168 0 1)))}

@defthing[ipv4-address/p (parser/c char? ipv4-address?)]{
 A @racketmodname[megaparsack] parser that consumes characters and produces an
 @racket[ipv4-address]. Like @racket[string->ipv4-address], parsing fails if
 the given components of the address are invalid.
 @(ip-examples
   (parse-string ipv4-address/p "127.0.0.1"))}

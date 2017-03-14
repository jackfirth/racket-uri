#lang scribble/manual

@(require syntax/parse/define)

@(define-simple-macro (racketmodlist (id:id pre-content ...) ...)
   (itemlist (item (racketmodname id) pre-content ...) ...))

@title{Parsing Networking Data Structures}
@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

The @racket[net-parse] package provides data structures and
@racketmodname[megaparsack] parsers for common networking data structures.
Included are parsers for IP addresses, DNS addresses, and URIs. This library is
intended for use when these values need to be handled in a more structured way
than freeform strings. This library also serves as an alternative API to the
functionality of the @racketmodname[net/url-structs] and
@racketmodname[net/url-string] modules, with more spec compliance and a focus on
supporting more URI schemes.

This package provides functionality via a handful of individual modules:

@racketmodlist[
 @net/ip/parse{Parsers and structures for IPv4 and IPv6 addresses}
 @net/dns/parse{Parsers and structures for DNS addresses}
 @net/uri/parse{Parsers and structures for generic URIs}]

@include-section["net-parse-ip.scrbl"]
@include-section["net-parse-dns.scrbl"]
@include-section["net-parse-uri.scrbl"]

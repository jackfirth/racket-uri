#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [plain-uri-path/p (parser/c char? uri-path?)]))

(require compose-app/fancy-app
         data/applicative
         data/functor
         megaparsack
         megaparsack/text
         net/uri/base
         "applicative.rkt"
         "string-parse.rkt")

(module+ test
  (require data/either
           rackunit))


(define path-sep/p (char/p #\/))

(define same-level/p (seq (char/p #\.) (pure same-level)))
(define up-one-level/p (seq (string/p "..") (pure same-level)))
(define empty-segment/p (pure empty-segment))

(define segment-nonempty/p (or/p _ same-level/p up-one-level/p))
(define segment/p (or/p _ empty-segment/p .. segment-nonempty/p))

(define or-empty/p (or/p _ (pure '())))

(define trailing-path/p
  (seq path-sep/p _ .. many/p _ #:sep path-sep/p .. segment/p))

(define (rootless-uri-path/p p)
  ((pure cons) (segment-nonempty/p p) (or-empty/p (trailing-path/p p))))

(define (absolute-uri-path/nonempty/p p)
  ((pure cons) (seq path-sep/p (segment-nonempty/p p))
               (or-empty/p (trailing-path/p p))))

(define (uri-path/p p)
  (map (apply uri-path _)
       (or/p (trailing-path/p p)
             (absolute-uri-path/nonempty/p p)
             (rootless-uri-path/p p)
             (pure '()))))

(define uri-path-char/p
  (or/p uri-unreserved/p pct-decode/p uri-sub-delims/p (char-in/p ":@")))

(define plain-segment/p
  (map (plain-segment .. list->string) (many+/p uri-path-char/p)))

(define plain-uri-path/p (uri-path/p plain-segment/p))

(module+ test
  (check-equal? (parse-string plain-uri-path/p "") (success empty-uri-path))
  (check-equal? (parse-string plain-uri-path/p "/")
                (success (uri-path empty-segment)))
  (check-equal? (parse-string plain-uri-path/p "/foo/bar/baz")
                (success (plain-uri-path "foo" "bar" "baz")))
  (check-equal? (parse-string plain-uri-path/p "foooo/foo/bar/baz")
                (success (plain-uri-path "foooo" "foo" "bar" "baz")))
  (check-equal? (parse-string plain-uri-path/p "fo:ooo/foo/bar/baz")
                (success (plain-uri-path "fo:ooo" "foo" "bar" "baz")))
  (check-equal? (parse-string plain-uri-path/p "/foo//bar")
                (success (uri-path (plain-segment "foo")
                                   empty-segment
                                   (plain-segment "bar"))))
  (check-equal? (parse-string plain-uri-path/p "/foo/bar/")
                (success (uri-path (plain-segment "foo")
                                   (plain-segment "bar")
                                   empty-segment))))

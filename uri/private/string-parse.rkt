#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [pct-decode/p (parser/c char? char?)]
  [stringof (-> (parser/c char? char?) (parser/c char? string?))]
  [symbol/p (-> symbol? (parser/c char? symbol?))]
  [uri-sub-delims/p (parser/c char? char?)]
  [uri-sub-delims/no-key-value/p (parser/c char? char?)]
  [uri-unreserved/p (parser/c char? char?)]))

(require compose-app/fancy-app
         data/applicative
         data/functor
         megaparsack
         megaparsack/text
         "applicative.rkt")

(module+ test
  (require data/either
           rackunit))


(define symbol/p (map string->symbol _ .. string/p .. symbol->string))
(define stringof (map list->string _ .. many/p))

(define (pct-decode first-hex second-hex)
  (integer->char (+ (* first-hex 16) second-hex)))

(define (hexchar->integer hexchar)
  (case hexchar
    [(#\0) 0] [(#\1) 1] [(#\2) 2] [(#\3) 3]
    [(#\4) 4] [(#\5) 5] [(#\6) 6] [(#\7) 7]
    [(#\8) 8] [(#\9) 9] [(#\a #\A) 10] [(#\b #\B) 11]
    [(#\c #\C) 12] [(#\d #\D) 13] [(#\e #\E) 14] [(#\f #\F) 15]))

(define hex-alphabet "0123456789abcdefABCDEF")

(define hexdigit/p
  (map hexchar->integer (char-in/p hex-alphabet)))

(module+ test
  (check-equal? (parse-string (many/p hexdigit/p) hex-alphabet)
                (success
                 (list 0 1 2 3 4 5 6 7 8 9
                       10 11 12 13 14 15
                       10 11 12 13 14 15))))

(define pct-decode/p
  (seq (char/p #\%) ((pure pct-decode) hexdigit/p hexdigit/p)))

(module+ test
  (check-equal? (parse-string (stringof pct-decode/p) "%48%65%6c%6C%6f%21")
                (success "Hello!")))

(define uri-alpha/p (or/p (char-between/p #\a #\z) (char-between/p #\A #\Z)))
(define uri-digit/p (char-between/p #\0 #\9))
(define uri-unreserved/p (or/p uri-alpha/p uri-digit/p (char-in/p "-._~")))
(define uri-sub-delims/p (char-in/p  "!$&'()*+,;="))
(define uri-sub-delims/no-key-value/p (char-in/p "!$'()*+,;"))

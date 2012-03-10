#lang racket/base
(require "simple-arithmetic-grammar.rkt"
         parser-tools/lex
         racket/list
         rackunit)

(define (lex ip)
  (lambda ()
    (cond [(regexp-match #px"^\\d+" ip)
           =>
           (lambda (a-match)
             (printf "here: ~a\n" (peek-byte ip))
             (token-INT (string->number (bytes->string/utf-8 (first a-match)))))]
          [else
           (printf "there: ~a\n" (peek-byte ip))
           (default-lex/1 ip)])))

(define t (lex (open-input-string "17 plus 4")))
(t)
(position-token-token (t))
(position-token-token (t))



;(parse #f (lex (open-input-string "42")))
;(parse #f (lex (open-input-string "3+4")))
;(parse #f (lex (open-input-string "4*5+6")))
;(check-exn exn:fail:parsing? (lambda () (parse #f (lex (open-input-string "7+")))))

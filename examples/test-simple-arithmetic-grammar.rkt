#lang racket/base
(require "simple-arithmetic-grammar.rkt"
         parser-tools/lex
         racket/list
         rackunit)

(define (tokenize ip)
  (define lex/1
    (lexer-src-pos
     [(repetition 1 +inf.0 numeric)
      (token-INT (string->number lexeme))]
     [whitespace
      (lex/1 ip)]
     ["+"
      (token-+ "+")]
     ["*"
      (token-+ "*")]
     [(eof)
      (token-EOF eof)]))
  (lambda ()
    (lex/1 ip)))


;; expr : term ('+' term)*
;; term : factor (('*') factor)*
;; factor : INT

(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "42"))))
              '(expr (term (factor 42))))
(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "3+4"))))
              '(expr (term (factor 3))
                     "+"
                     (term (factor 3))))

;; (parse #f (tokenize (open-input-string "4*5+6")))


(check-exn exn:fail:parsing?
           (lambda () (parse #f (tokenize (open-input-string "7+")))))

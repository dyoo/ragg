#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "parser.rkt"
         rackunit)

(provide lex/1 tokenize)

(define lex/1
  (lexer-src-pos
   [(:+ (:or alphabetic numeric))
    (token-ID lexeme)]
   [(:: "'" (:+ (complement "'")) "'")
    (token-ID lexeme)]
   [(:or "(" "[")
    (token-LPAREN lexeme)]
   [(:or ")" "]")
    (token-RPAREN lexeme)]
   [":"
    (token-COLON lexeme)]
   ["|"
    (token-PIPE lexeme)]
   [(:or "+" "*")
    (token-REPEAT lexeme)]
   [whitespace
    (return-without-pos (lex/1 input-port))]
   [(eof)
    (token-EOF lexeme)]))

;; tokenize: input-port -> (-> token)
(define (tokenize ip)
  (lambda ()
    (lex/1 ip)))



(define (l s)
  (define t (lex/1 (open-input-string s)))
  (list (token-name (position-token-token t))
        (token-value (position-token-token t))
        (position-offset (position-token-start-pos t))
        (position-offset (position-token-end-pos t))))
                             

(check-equal? (l " hi")
              '(ID "hi" 2 4))

(check-equal? (l "  hi")
              '(ID "hi" 3 5))
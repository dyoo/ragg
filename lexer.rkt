#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "parser.rkt")

(provide lex/1 tokenize)

(define lex/1
  (lexer-src-pos
   [(:+ (:or alphabetic numeric))
    (token-ID lexeme)]
   [(:: "'" (:+ (complement "'")) "'")
    (token-LIT lexeme)]
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



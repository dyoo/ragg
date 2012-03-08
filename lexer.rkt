#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "parser.rkt")

(provide (all-defined-out))

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
   [(eof)
    (token-EOF lexeme)]))

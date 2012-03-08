#lang racket/base
(require parser-tools/yacc
         parser-tools/lex)

;; A parser for grammars.

(provide (all-defined-out))

(define-tokens tokens (LPAREN
                       RPAREN
                       COLON
                       PIPE
                       REPEAT
                       ID
                       STRING
                       EOF))

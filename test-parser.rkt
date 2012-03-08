#lang racket/base

(require rackunit
         "parser.rkt"
         "lexer.rkt")

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'")))
              (list (rule (lhs-id "expr" 1 5)
                          (rhs-lit "'hello'" 8 15))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON")))
              (list (rule (lhs-id "expr" 1 5)
                          (rhs-token "COLON" 8 13))))

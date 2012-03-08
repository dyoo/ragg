#lang racket/base

(require rackunit
         "parser.rkt"
         "lexer.rkt")

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'")))
              (list (rule (lhs-id 1 5 "expr" )
                          (rhs-lit 8 15 "'hello'"))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON")))
              (list (rule (lhs-id 1 5 "expr")
                          (rhs-token 8 13 "COLON"))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON COLON")))
              (list (rule (lhs-id 1 5 "expr")
                          (rhs-seq 8 19
                                   (list
                                    (rhs-token 8 13 "COLON")
                                    (rhs-token 14 19 "COLON"))))))

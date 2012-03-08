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


(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH")))
              (list (rule (lhs-id 1 5 "expr")
                          (rhs-choice 8 20
                                      (rhs-token 8 13 "COLON")
                                      (rhs-token 16 20 "BLAH")))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH | BAZ expr")))
              (list (rule (lhs-id 1 5 "expr")
                          (rhs-choice 8 31
                                      (rhs-token 8 13 "COLON")
                                      (rhs-choice 16 31
                                                  (rhs-token 16 20 "BLAH")
                                                  (rhs-seq 23 31
                                                           (list (rhs-token 23 26 "BAZ")
                                                                 (rhs-id 27 31 "expr"))))))))

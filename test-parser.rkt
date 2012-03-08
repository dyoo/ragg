#lang racket/base

(require rackunit
         "parser.rkt"
         "lexer.rkt")

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'")))
              (list (rule 1 15
                          (lhs-id 1 5 "expr" )
                          (rhs-lit 8 15 "'hello'"))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON")))
              (list (rule 1 13
                          (lhs-id 1 5 "expr")
                          (rhs-token 8 13 "COLON"))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON COLON")))
              (list (rule 1 19
                          (lhs-id 1 5 "expr")
                          (rhs-seq 8 19
                                   (list
                                    (rhs-token 8 13 "COLON")
                                    (rhs-token 14 19 "COLON"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'*")))
              (list (rule 1 16
                          (lhs-id 1 5 "expr" )
                          (rhs-repeat 8 16
                                      0
                                      (rhs-lit 8 15 "'hello'")))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'+")))
              (list (rule 1 16
                          (lhs-id 1 5 "expr" )
                          (rhs-repeat 8 16
                                      1
                                      (rhs-lit 8 15 "'hello'")))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : ['hello']")))
              (list (rule 1 17
                          (lhs-id 1 5 "expr" )
                          (rhs-maybe 8 17
                                     (rhs-lit 9 16 "'hello'")))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH")))
              (list (rule 1 20
                          (lhs-id 1 5 "expr")
                          (rhs-choice 8 20
                                      (list (rhs-token 8 13 "COLON")
                                            (rhs-token 16 20 "BLAH"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH | BAZ expr")))
              (list (rule 1 31
                          (lhs-id 1 5 "expr")
                          (rhs-choice 8 31
                                      (list (rhs-token 8 13 "COLON")
                                            (rhs-token 16 20 "BLAH")
                                            (rhs-seq 23 31
                                                     (list (rhs-token 23 26 "BAZ")
                                                           (rhs-id 27 31 "expr"))))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two three")))
              (list (rule 1 21
                          (lhs-id 1 5 "expr")
                          (rhs-seq 8 21 (list (rhs-id 8 11 "one")
                                              (rhs-id 12 15 "two")
                                              (rhs-id 16 21 "three"))))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two three)")))
              (list (rule 1 23
                          (lhs-id 1 5 "expr")
                          (rhs-seq 8 23 (list (rhs-id 9 12 "one")
                                              (rhs-id 13 16 "two")
                                              (rhs-id 17 22 "three"))))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two* three")))
              (list (rule 1 22
                          (lhs-id 1 5 "expr")
                          (rhs-seq 8 22 (list (rhs-id 8 11 "one")
                                              (rhs-repeat 12 16 0 (rhs-id 12 15 "two"))
                                              (rhs-id 17 22 "three"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two+ three")))
              (list (rule 1 22
                          (lhs-id 1 5 "expr")
                          (rhs-seq 8 22 (list (rhs-id 8 11 "one")
                                              (rhs-repeat 12 16 1 (rhs-id 12 15 "two"))
                                              (rhs-id 17 22 "three"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two)+ three")))
              (list (rule 1 24
                          (lhs-id 1 5 "expr")
                          (rhs-seq 8 24 (list (rhs-repeat 8 18 1
                                                          (rhs-seq 8 17
                                                                   (list (rhs-id 9 12 "one")
                                                                         (rhs-id 13 16 "two"))))
                                              (rhs-id 19 24 "three"))))))


(check-equal? (grammar-parser (tokenize (open-input-string #<<EOF
statlist : stat+
stat: ID '=' expr
    | 'print' expr
EOF
)))
              (list (rule 1 17
                          (lhs-id 1 9 "statlist")
                          (rhs-repeat 12 17 1 (rhs-id 12 16 "stat")))
                    (rule 18 54
                          (lhs-id 18 22 "stat")
                          (rhs-choice 24 54 (list (rhs-seq 24 35 (list (rhs-token 24 26 "ID")
                                                                       (rhs-lit 27 30 "'='")
                                                                       (rhs-id 31 35 "expr")))
                                                  (rhs-seq 42 54 (list (rhs-lit 42 49 "'print'")
                                                                       (rhs-id 50 54 "expr"))))))))


(let ([parsed
       (call-with-input-file "python-grammar.rkt"
         (lambda (ip)
           (port-count-lines! ip)
           (grammar-parser (tokenize ip))))])
  (for ([rule parsed]) (displayln rule) (newline)))
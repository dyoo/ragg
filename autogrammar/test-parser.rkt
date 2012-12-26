#lang racket/base


(require rackunit
         parser-tools/lex
         "parser.rkt"
         "lexer.rkt"
         "rule-structs.rkt")


;; quick-and-dirty helper for pos construction.
(define (p x)
  (pos x #f #f))



;; FIXME: fix the test cases so they work on locations rather than just offsets.
(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'")))
              (list (rule (p 1) (p 15)
                          (lhs-id (p 1) (p 5) "expr" )
                          (pattern-lit (p 8) (p 15) "hello"))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON")))
              (list (rule 1 13
                          (lhs-id 1 5 "expr")
                          (pattern-token 8 13 "COLON"))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON COLON")))
              (list (rule 1 19
                          (lhs-id 1 5 "expr")
                          (pattern-seq 8 19
                                   (list
                                    (pattern-token 8 13 "COLON")
                                    (pattern-token 14 19 "COLON"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'*")))
              (list (rule 1 16
                          (lhs-id 1 5 "expr" )
                          (pattern-repeat 8 16
                                      0
                                      (pattern-lit 8 15 "'hello'")))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'+")))
              (list (rule 1 16
                          (lhs-id 1 5 "expr" )
                          (pattern-repeat 8 16
                                      1
                                      (pattern-lit 8 15 "'hello'")))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : ['hello']")))
              (list (rule 1 17
                          (lhs-id 1 5 "expr" )
                          (pattern-maybe 8 17
                                     (pattern-lit 9 16 "'hello'")))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH")))
              (list (rule 1 20
                          (lhs-id 1 5 "expr")
                          (pattern-choice 8 20
                                      (list (pattern-token 8 13 "COLON")
                                            (pattern-token 16 20 "BLAH"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH | BAZ expr")))
              (list (rule 1 31
                          (lhs-id 1 5 "expr")
                          (pattern-choice 8 31
                                      (list (pattern-token 8 13 "COLON")
                                            (pattern-token 16 20 "BLAH")
                                            (pattern-seq 23 31
                                                     (list (pattern-token 23 26 "BAZ")
                                                           (pattern-id 27 31 "expr"))))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two three")))
              (list (rule 1 21
                          (lhs-id 1 5 "expr")
                          (pattern-seq 8 21 (list (pattern-id 8 11 "one")
                                              (pattern-id 12 15 "two")
                                              (pattern-id 16 21 "three"))))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two three)")))
              (list (rule 1 23
                          (lhs-id 1 5 "expr")
                          (pattern-seq 8 23 (list (pattern-id 9 12 "one")
                                              (pattern-id 13 16 "two")
                                              (pattern-id 17 22 "three"))))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two* three")))
              (list (rule 1 22
                          (lhs-id 1 5 "expr")
                          (pattern-seq 8 22 (list (pattern-id 8 11 "one")
                                              (pattern-repeat 12 16 0 (pattern-id 12 15 "two"))
                                              (pattern-id 17 22 "three"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two+ three")))
              (list (rule 1 22
                          (lhs-id 1 5 "expr")
                          (pattern-seq 8 22 (list (pattern-id 8 11 "one")
                                              (pattern-repeat 12 16 1 (pattern-id 12 15 "two"))
                                              (pattern-id 17 22 "three"))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two)+ three")))
              (list (rule 1 24
                          (lhs-id 1 5 "expr")
                          (pattern-seq 8 24 (list (pattern-repeat 8 18 1
                                                          (pattern-seq 8 17
                                                                   (list (pattern-id 9 12 "one")
                                                                         (pattern-id 13 16 "two"))))
                                              (pattern-id 19 24 "three"))))))


(check-equal? (grammar-parser (tokenize (open-input-string #<<EOF
statlist : stat+
stat: ID '=' expr
    | 'print' expr
EOF
)))
              (list (rule 1 17
                          (lhs-id 1 9 "statlist")
                          (pattern-repeat 12 17 1 (pattern-id 12 16 "stat")))
                    (rule 18 54
                          (lhs-id 18 22 "stat")
                          (pattern-choice 24 54 (list (pattern-seq 24 35 (list (pattern-token 24 26 "ID")
                                                                       (pattern-lit 27 30 "'='")
                                                                       (pattern-id 31 35 "expr")))
                                                  (pattern-seq 42 54 (list (pattern-lit 42 49 "'print'")
                                                                       (pattern-id 50 54 "expr"))))))))


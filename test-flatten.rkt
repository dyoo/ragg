#lang racket/base
(require "stx-types.rkt"
         "flatten.rkt"
         rackunit)

(check-equal? (map syntax->datum (flatten-rule #'(rule expr (lit "hello"))))
              '((prim-rule expr [(lit "hello")])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr
                                         (seq (lit "hello")
                                              (lit "world")))))
              '((prim-rule expr [(lit "hello") (lit "world")])))


(check-equal? (map syntax->datum (flatten-rule #'(rule expr (token HELLO))))
              '((prim-rule expr [(token HELLO)])))

(check-equal? (map syntax->datum (flatten-rule #'(rule expr (id rule-2))))
              '((prim-rule expr [(id rule-2)])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (choice (id rule-2) (id rule-3)))))
              '((prim-rule expr
                           [(id rule-2)]
                           [(id rule-3)])))


(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (maybe (id rule-2)))))
              '((prim-rule expr
                           [(id rule-2)]
                           [])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 0 (id rule-2)))))
              '((prim-rule rule-2+
                           [(id rule-2) (id rule-2+)]
                           [])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 1 (id rule-2)))))
              '((prim-rule rule-2+
                           [(id rule-2) (id rule-2+)]
                           [(id rule-2)])))







;(flatten-rule #'(rule expr (lit "hello") (lit "world")))


;(flatten-rule #'(rule expr (choice (lit "hello")
;                                   (lit "world"))))

;; I want to get back
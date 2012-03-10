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


(define (make-fresh-name)
  (let ([n 0])
    (lambda ()
      (set! n (add1 n))
      (string->symbol (format "r~a" n)))))


(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (seq (lit "(") (lit ")"))
                                                      (seq)))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule sexp
                           [(inferred-id r1)] [(inferred-id r2)])
                (inferred-prim-rule r1
                           [(lit "(") (lit ")")])
                (inferred-prim-rule r2
                           [])))


(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (lit "x")
                                                      (maybe (lit "y"))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule sexp
                           [(lit "x")]
                           [(inferred-id r1)])
                (inferred-prim-rule r1
                           [(lit "y")]
                           [])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (lit "x")
                                                      (maybe (repeat 1 (lit "y")))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule sexp
                           [(lit "x")]
                           [(inferred-id r1)])
                (inferred-prim-rule r1
                           [(inferred-id r2)]
                           [])
                (inferred-prim-rule r2
                           [(lit "y") (inferred-id r2)]
                           [(lit "y")])))








;(flatten-rule #'(rule expr (lit "hello") (lit "world")))


;(flatten-rule #'(rule expr (choice (lit "hello")
;                                   (lit "world"))))

;; I want to get back
#lang racket/base
(require "../stx-types.rkt"
         "flatten.rkt"
         rackunit)


(define (make-fresh-name)
  (let ([n 0])
    (lambda ()
      (set! n (add1 n))
      (string->symbol (format "r~a" n)))))


;; Simple literals
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


;; Sequences of primitives
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (lit "1") (seq (lit "2") (lit "3"))))))
              '((prim-rule expr 
                          [(lit "1") (lit "2") (lit "3")])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (seq (lit "1") (lit "2")) (lit "3")))))
              '((prim-rule expr 
                          [(lit "1") (lit "2") (lit "3")])))


(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (seq (lit "1")) (seq (lit "2") (lit "3"))))))
              '((prim-rule expr 
                          [(lit "1") (lit "2") (lit "3")])))



;; choices
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (choice (id rule-2) (id rule-3)))))
              '((prim-rule expr
                           [(id rule-2)]
                           [(id rule-3)])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (seq (lit "(") (lit ")"))
                                                      (seq)))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule sexp
                           [(lit "(") (lit ")")] [])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (seq (seq (lit "(") (token BLAH))
                                                           (lit ")"))
                                                      (seq)))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule sexp
                           [(lit "(") (token BLAH) (lit ")")] [])))




;; maybe
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (maybe (id rule-2)))))
              '((prim-rule expr
                           [(id rule-2)]
                           [])))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (maybe (token HUH)))))
              '((prim-rule expr
                           [(token HUH)]
                           [])))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (maybe (seq (lit "hello") (lit "world"))))))
              '((prim-rule expr
                           [(lit "hello") (lit "world")]
                           [])))




;; repeat
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 0 (id rule-2)))))
              '((prim-rule rule-2+
                           [(id rule-2+) (id rule-2)]
                           [])))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 0 (seq (lit "+") (id rule-2))))))
              '((prim-rule rule-2+
                           [(id rule-2+) (lit "+") (id rule-2)]
                           [])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 1 (id rule-2)))))
              '((prim-rule rule-2+
                           [(id rule-2+) (id rule-2)]
                           [(id rule-2)])))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 1 (seq (lit "-") (id rule-2))))))
              '((prim-rule rule-2+
                           [(id rule-2+) (lit "-") (id rule-2)]
                           [(lit "-") (id rule-2)])))






;; Mixtures

;; choice and maybe
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
;; choice, maybe, repeat
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
                           [(inferred-id r2) (lit "y")]
                           [(lit "y")])))
;; choice, seq
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (seq (lit "x") (lit "y"))
                                                      (seq (lit "z") (lit "w"))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule sexp
                           [(lit "x") (lit "y")]
                           [(lit "z") (lit "w")])))

;; maybe, choice
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (maybe (choice (seq (lit "x") (lit "y"))
                                                             (seq (lit "z") (lit "w")))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule sexp
                           [(inferred-id r1)]
                           [])
                (inferred-prim-rule r1
                                    [(lit "x") (lit "y")]
                                    [(lit "z") (lit "w")])))


;; seq, repeat
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (id term) (repeat 0 (seq (lit "+") (id term)))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule expr [(id term) (inferred-id r1)])
                (inferred-prim-rule r1 [(inferred-id r1) (lit "+") (id term)] [])))


;; larger example: simple arithmetic
(check-equal? (map syntax->datum
                   (flatten-rules (syntax->list
                                   #'((rule expr (seq (id term) (repeat 0 (seq (lit "+") (id term)))))
                                      (rule term (seq (id factor) (repeat 0 (seq (lit "*") (id factor)))))
                                      (rule factor (token INT))))
                                  #:fresh-name (make-fresh-name)))
              
              '((prim-rule expr [(id term) (inferred-id r1)])
                (inferred-prim-rule r1 [(inferred-id r1) (lit "+") (id term)] [])
                (prim-rule term [(id factor) (inferred-id r2)])
                (inferred-prim-rule r2 [(inferred-id r2) (lit "*") (id factor)] [])
                (prim-rule factor [(token INT)])))

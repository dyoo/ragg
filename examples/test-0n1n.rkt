#lang racket/base
(require "0n1n.rkt"
         rackunit)


(define (lex ip)
  (port-count-lines! ip)
  (lambda ()
    (default-lex/1 ip)))

;; The only rule in the grammar is:
;;
;;      rule-0n1n: ["0" rule-0n1n "1"]
;;
;; It makes use of the "maybe" pattern.  The result type of the
;; grammar rule is:
;;
;; rule-0n1n: (U #f
;;               (list "0" rule-0n1n "1"))

(check-equal? (syntax->datum (parse #f (lex (open-input-string "0011"))))
              '(rule-0n1n "0" (rule-0n1n "0" #f "1") "1"))

(check-equal? (syntax->datum (parse #f (lex (open-input-string "01"))))
              '(rule-0n1n "0" #f "1"))

(check-equal? (syntax->datum (parse #f (lex (open-input-string ""))))
              #f)

(check-equal? (syntax->datum (parse #f (lex (open-input-string "000111"))))
              '(rule-0n1n "0" (rule-0n1n "0" (rule-0n1n "0" #f "1") "1") "1"))



(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "0001111")))))

(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "0001110")))))

(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "10001110")))))
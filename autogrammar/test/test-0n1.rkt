#lang racket/base

(require "../examples/0n1.rkt"
         rackunit)

(define (lex ip)
  (port-count-lines! ip)
  (lambda ()
    (default-lex/1 ip)))


(check-equal? (syntax->datum (parse #f (lex (open-input-string "1"))))
              '(rule "1"))


(check-equal? (syntax->datum (parse #f (lex (open-input-string "01"))))
              '(rule "0" "1"))


(check-equal? (syntax->datum (parse #f (lex (open-input-string "001"))))
              '(rule "0" "0" "1"))


(check-exn exn:fail:parsing?
           (lambda ()
             (parse #f (lex (open-input-string "0")))))

(check-exn exn:fail:parsing?
           (lambda ()
             (parse #f (lex (open-input-string "10")))))

(check-exn exn:fail:parsing?
           (lambda ()
             (parse #f (lex (open-input-string "010")))))

